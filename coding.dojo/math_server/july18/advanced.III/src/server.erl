-module(server).
-export([start/0, start/1, stop/0]).
-export([check_resources/0]).
-export([init/1]).
-include("server.hrl").

start() ->
    start(available_resources()).

start(Args) ->
    Pid = spawn(?MODULE, init, [{Args, fun geometry:areas/1}]),
    register(?Server, Pid),
    {ok, noreply}.

stop() ->
    ?Server ! {stop, self()},
    {ok, noreply}.

check_resources() ->
    ?Server ! {request, self(), stats},
    receive
	Reply ->
	    Reply
    end.

init({InitialState, Callback}) ->
    process_flag(trap_exit, true),
    loop(InitialState, Callback).

loop(State, Callback) ->
    receive
	{'EXIT', From, _Why} ->
	    {NewState, _Reply} = disconnect_client(State, From),
	    loop(NewState, Callback);
	{stop, _From} ->
	    deallocate_all(),
	    unregister(?Server),
	    exit(stop);
	{sum_areas, Shapes, Pid} ->
	    Result = eval(Callback, Shapes),
	    Pid ! {reply, Result},
	    loop(State, Callback);
	{connect, Client} ->
	    {NewState, Reply} = connect_client(State, Client),
	    Client ! {reply, ?Server, Reply},
	    loop(NewState, Callback);
	{disconnect, Client} ->
	    {NewState, Reply} = disconnect_client(State, Client),
	    Client ! {reply, ?Server, Reply},
	    loop(NewState, Callback);
	{request, From, stats} ->
	    From ! {reply, ?Server, State},
	    loop(State, Callback)
    end.

eval(Fun, Args) ->
    case catch Fun(Args) of
	{'EXIT', Why} ->
	    {error, Why};
	Sum ->
	    {ok, Sum}
    end.

connect_client(State, ClientPid) ->
    case allocate(State, ClientPid) of
	{error, exhausted} ->
	    {State, {error, connect, 'no enough resources'}};
	{error, already_allocated} = ALC ->
	    {State, ALC};
	{ok, NewAllocations} ->
	    link(ClientPid),
	    {NewAllocations, {ok, connect, connected}}
    end.

disconnect_client(State, ClientPid) ->
    case deallocate(State, ClientPid) of
	{error, not_allocated} = NALC ->
	    {State, NALC};
	{ok, NewAllocations} ->
	    unlink(ClientPid),
	    {NewAllocations, {ok, disconnected}}
    end.

deallocate_all() ->
    available_resources().

allocate(Resources, Pid) ->
    Result = case lists:keyfind(Pid, 2, Resources) of
		 {_Res, Pid} ->
		     {error, already_allocated};
		 false ->
		     case lists:keyfind(undefined, 2, Resources) of
			 {Res, undefined} ->
			     NewAllocations = lists:keyreplace(undefined, 2, Resources, {Res, Pid}),
			     {ok, NewAllocations};
			 false ->
			     {error, exhausted}
		     end
	     end,
    Result.

deallocate(Resources, Pid) ->
    Result = case lists:keyfind(Pid, 2, Resources) of
		 {Res, Pid} ->
		     NewAllocations = lists:keyreplace(Pid, 2, Resources, {Res, undefined}),
		     {ok, NewAllocations};
		 false ->
		     {error, not_allocated}
	     end,
    Result.

available_resources() ->
    [{'0x8736', undefined}, {'0x34a', undefined}].%, {'0x4114', undefined}, {'0x1235',undefined}].
