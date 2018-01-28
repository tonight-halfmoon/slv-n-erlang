%%%-------------------------------------------------------------------
%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% On the 22nd of Jan 2018
%%%-------------------------------------------------------------------
-module(riakc_geocheckin).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1,
	 write/0, read/0, delete/0,
	 truncate/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(griakc, riakc_geocheckin_proc).

-record(state, {riakc_socket_proc, riak_ip, riak_port}).
-record(to_geocheckin, {id, time, region, state, weather, temperature}).
-record(geocheckin_pk, {id, time}).
-record(query_geocheckin, {select = [], where = #geocheckin_pk{}}).
-record(del_row_in_geocheckin, {where = #geocheckin_pk{}}).
-record(truncate, {}).

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
    gen_server:start_link({local, ?griakc}, ?MODULE, [{RiakIP, RiakPort}], [{debug, [trace, statistics]}]).

write() ->
    gen_server:cast(?griakc, #to_geocheckin{id = 6, time = 1451606402, region = <<"Brazil">>, state = <<"Amazon">>, weather = <<"extremely hot">>, temperature = 33.4}).

read() ->
    gen_server:call(?griakc, #query_geocheckin{select = [weather, temperature], where = #geocheckin_pk{id = 6, time = {1441606401, 1585606401}}}).

delete() ->
    gen_server:cast(?griakc, #del_row_in_geocheckin{where = #geocheckin_pk{id = 6, time = 1451606402}}).

truncate() ->
    gen_server:cast(?griakc, #truncate{}).

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
handle_call(#query_geocheckin{select = [_C1, _C2], where = #geocheckin_pk{id = _Id, time = {_T1, _T2}}} = Query_GeoCheckin, _From, State = #state{riakc_socket_proc = nil, riak_ip = Riak_ip, riak_port = Riak_port}) ->
    case riakc_pb_socket:start_link(Riak_ip, Riak_port) of
	Result = {error, {tcp, ehostunreach}} ->
	    {reply, Result, State};
	{ok, Pid} ->
	    NewState = #state{riakc_socket_proc = Pid, riak_ip = Riak_ip, riak_port = Riak_port},
	    Result = read_internal(State, Query_GeoCheckin),
	    {reply, Result, NewState}
    end;
handle_call(#query_geocheckin{select = [_C1, _C2], where = #geocheckin_pk{id = _Id, time = {_T1, _T2}}} = Query_GeoCheckin, _From, State) ->
    Result = read_internal(State, Query_GeoCheckin),
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
handle_cast(#to_geocheckin{id = Id, time = Time, region = R, state = S, weather = W, temperature = Temperature}, State = #state{riakc_socket_proc = nil, riak_ip = Riak_ip, riak_port = Riak_port}) ->
    case riakc_pb_socket:start_link(Riak_ip, Riak_port) of
	{error, {tcp, ehostunreach}} ->
	    {noreply, State};
	{ok, Pid} ->
	    NewState = #state{riakc_socket_proc = Pid, riak_ip = Riak_ip, riak_port = Riak_port},
	    write_internal(NewState, {Id, Time, R, S, W, Temperature}),
	    {noreply, NewState}
    end;
handle_cast(#to_geocheckin{id = Id, time = Time, region = R, state = S, weather = W, temperature = Temperature}, State = #state{riakc_socket_proc = _Pid, riak_ip = _Riak_ip, riak_port = _Riak_port}) ->
    write_internal(State, {Id, Time, R, S, W, Temperature}),
    {noreply, State};
handle_cast(Query_GeoCheckin, State = #state{riakc_socket_proc = Pid, riak_ip = _Riak_ip, riak_port = _Riak_port}) ->
    delete_internal(Pid, Query_GeoCheckin),
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
    io:format("~p received info 'EXIT' from ~p for ~p~n", [?griakc, Pid, Reason]),
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

write_internal(#state{riakc_socket_proc = Pid, riak_ip = _Riak_ip, riak_port = _Riak_port}, {Id, Time, R, S, W, Temperature}) ->
    riakc_ts:put(Pid, "GeoCheckin", [{Id, Time, R, S, W, Temperature}]).

read_internal(#state{riakc_socket_proc = Pid, riak_ip = _Riak_ip, riak_port = _Riak_port}, #query_geocheckin{select = [Weather, Temperature], where = #geocheckin_pk{id = Id, time = {T1, T2}}}) ->
    riakc_ts:query(Pid, lists:concat(["select ", Weather, ", ", Temperature, " from ", "GeoCheckin", " where id = ", Id, " and time > ", T1, " and time < ", T2])).

delete_internal(RiakcSocketPid, #del_row_in_geocheckin{where = #geocheckin_pk{id = Id, time = Time}}) ->
    riakc_ts:query(RiakcSocketPid, lists:concat(["delete from ", "GeoCheckin", " where id = ", Id, " and time = ", Time])).
