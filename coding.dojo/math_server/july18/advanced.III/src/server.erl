-module(server).
-export([start/0, start/1, stop/0]).
-export([allocated/0]).
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

allocated() ->
    ?Server ! {request, self(), stats},
    receive
	Reply ->
	    Reply
    end.

init({InitialState, Callback}) ->
    process_flag(trap_exit, true),
    loop(InitialState, Callback).

available_resources() ->
    [{'0x8736', undefined}, {'0x34a', undefined}].%, {'0x4114', undefined}, {'0x1235',undefined}].

loop(State, Callback) ->
    receive
	{'EXIT', From, _Why} ->
	    NewState = disconnect_client(State, From),
	    loop(NewState, Callback);
	{stop, _From} ->
	    deallocate_all(),
	    unregister(?Server),
	    exit(stop);
	{sum_areas, Shapes, Pid} ->
	    Result = eval(Callback, Shapes),
	    Pid ! {reply, Result},
	    loop(State, Callback);
	{connect, ClientPid} ->
	    case connect_client(State, ClientPid) of
		{error, exhausted} = Reply ->
		    ClientPid ! {reply, ?Server, {Reply, not_connected}},
		    loop(State, Callback);
		NewState ->
		    link(ClientPid),
		    ClientPid ! {reply, ?Server, connected},
		    loop(NewState, Callback)
	    end;
	{disconnect, ClientPid} ->
	    NewState = disconnect_client(State, ClientPid),
	    unlink(ClientPid),
	    ClientPid ! {reply, ?Server, disconnected},
	    loop(NewState, Callback);
	{request, From, stats} ->
	    From ! {reply,?Server, State},
	    loop(State, Callback)
    end.

eval(Fun, Args) ->
    case catch Fun(Args) of
	{'EXIT', Why} ->
	    {error, Why};
	Sum ->
	    {ok, Sum}
    end.

connect_client(Resources, ClientPid) ->
    allocate(Resources, ClientPid).

disconnect_client(Resources, ClientPid) ->
    deallocate(Resources, ClientPid).

deallocate_all() ->
    available_resources().

allocate(Resources, Pid) ->
    Result = case lists:keyfind(Pid, 2, Resources) of
		 {_Res, Pid} ->
		     {error, already_allocated};
		 false ->
		     case lists:keyfind(undefined, 2, Resources) of
			 {Res, undefined} ->
			     lists:keyreplace(undefined, 2, Resources, {Res, Pid});
			 false ->
			     {error, exhausted}
		     end
	     end,
    Result.

deallocate(Resources, Pid) ->
    Result = case lists:keyfind(Pid, 2, Resources) of
		 {Res, Pid} ->
		     lists:keyreplace(Pid, 2, Resources, {Res, undefined});
		 false ->
		     {error, not_allocated}
	     end,
    Result.
