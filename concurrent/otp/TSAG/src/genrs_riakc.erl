%%%-------------------------------------------------------------------
%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% On the 22nd of Jan 2018
%%%-------------------------------------------------------------------
-module(genrs_riakc).

-behaviour(gen_server).

%% API
-export([start_link/0, store/3, wro_geocheckin/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(griakc, ?MODULE).

-record(state, {}).
-record(store_new, {bucket, key, value}).
-record(to_geocheckin, {id, time, region, state, weather, temperature}).

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
    gen_server:start_link({local, ?griakc}, ?MODULE, [], []).

store(Bucket, Key, Value) ->
    gen_server:cast(?griakc, #store_new{bucket= Bucket, key= Key, value = Value}).

wro_geocheckin() ->
    gen_server:cast(?griakc, #to_geocheckin{id = 1, time = 1224435679, region = <<"Brazil">>, state = <<"Amazon">>, weather = <<"extremely hot">>, temperature = 33.4}).

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
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

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
handle_cast(#store_new{bucket=Bucket, key=Key, value=Value}, State) ->
    store_internal(Bucket, Key, Value),
    {noreply, State};
handle_cast(#to_geocheckin{id = Id, time = T, region = R, state = S, weather = W, temperature=T}, State) ->
    wro_geocheckin_internal(Id, T, R, S, W, T),
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
terminate(_Reason, _State) ->
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

store_internal(Bucket, Key, Value) ->
    {ok, Pid} = riakc_pb_socket:start_link("172.17.0.2", 8087),
    riakc_pb_socket:put(Pid, riakc_obj:new(Bucket, Key, Value)).

wro_geocheckin_internal(Id, T, R, S, W, T) ->
    {ok, Pid} = riakc_pb_socket:start_link("172.17.0.2", 8087),
    R = riakc_ts:put(Pid, "GeoCheckin", [{Id, T, R, S, W, T}]),
    io:format("wrote to GeoCheckin with result ~p~n", [R]).
