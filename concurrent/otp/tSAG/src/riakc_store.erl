%%%-------------------------------------------------------------------
%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% On the 22nd of Jan 2018
%%%-------------------------------------------------------------------
-module(riakc_store).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, 
	 put/1, get/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(sriakc, riakc_store_proc).

-record(state, {riakc_socket_proc, riak_ip, riak_port}).
-record(store_new, {bucket, key, value}).
-record(store_get, {bucket, key}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    start_link({"172.17.0.2", 8087}).

start_link({RiakIP, RiakPort}) ->
    gen_server:start_link({local, ?sriakc}, ?MODULE, [{RiakIP, RiakPort}], [{debug, [trace, statistics]}]).

put({Bucket, Key, Value}) ->
    gen_server:cast(?sriakc, #store_new{bucket= Bucket, key= Key, value = Value}).

get({Bucket, Key}) ->
    gen_server:call(?sriakc, #store_get{bucket = Bucket, key = Key}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([{Riak_ip, Riak_port}]) ->
    process_flag(trap_exit, true),
    case riakc_pb_socket:start_link(Riak_ip, Riak_port) of
	{error, {tcp, ehostunreach}} ->
	    {warning, #state{riakc_socket_proc = nil, riak_ip = Riak_ip, riak_port = Riak_port}};
	{ok, Pid} ->
	    {ok, #state{riakc_socket_proc = Pid, riak_ip = Riak_ip, riak_port = Riak_port}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(#store_get{bucket = Bucket, key = Key}, _From, #state{riakc_socket_proc = Pid, riak_ip = _Riak_ip, riak_port = _Riak_port} = State) ->
    Result = get_internal(Pid, {Bucket, Key}),
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(#store_new{bucket=Bucket, key=Key, value=Value}, #state{riakc_socket_proc = Pid, riak_ip = _Riak_ip, riak_port = _Riak_port} = State) ->
    store_internal(Pid, Bucket, Key, Value),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, State) ->
    io:format("~p received info 'EXIT' from ~p for ~p~n", [?sriakc, Pid, Reason]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, _State) ->
    io:format("Shutdown because of ~p~n", [Reason]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

store_internal(RiakSocketPid, Bucket, Key, Value) ->
    riakc_pb_socket:put(RiakSocketPid, riakc_obj:new(Bucket, Key, Value)).

get_internal(RiakSocketPid, {Bucket, Key})->
    riakc_pb_socket:get(RiakSocketPid, Bucket, Key).
    
