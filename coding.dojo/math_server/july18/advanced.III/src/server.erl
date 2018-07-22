-module(server).
-export([start/0, stop/0]).
-export([init/1]).
-include("server.hrl").

start() ->
    Pid = spawn(?MODULE, init, [fun geometry:areas/1]),
    register(?Server, Pid),
    {ok, noreply}.

stop() ->
    ?Server ! {stop, self()},
    {ok, noreply}.

init(Args) ->
	process_flag(trap_exit, true),
    InitialState = initialise_state(),
    loop(InitialState, Args).
initialise_state() ->
	[{4, undefined}, {9, undefined}, {14, undefined}, {5,undefined}].

loop(State, Callback) ->
    receive
	{'EXIT', From, _Why} ->
		NewState = disconnect_client(State, From),
		loop(NewState, Callback);
	{stop, _From} ->
	    unregister(?Server),
	    exit(stop);
	{sum_areas, Shapes, Pid} ->
	    Result = eval(Callback, Shapes),
	    Pid ! {reply, Result},
	    loop(State, Callback);
	{connect, ClientPid} ->
	    NewState = connect_client(State, ClientPid),
		link(ClientPid),
	    ClientPid ! {reply, ?Server, connected},
	    loop(NewState, Callback);
	{disconnect, ClientPid} ->
		NewState = disconnect_client(State, ClientPid),
		unlink(ClientPid),
		ClientPid ! {reply, ?Server, disconnected},
		loop(NewState, Callback)
    end.

eval(Fun, Args) ->
    case catch Fun(Args) of
	{'EXIT', Why} ->
	    {error, Why};
	Sum ->
	    {ok, Sum}
    end.

connect_client([{Res, undefined}|T], ClientPid) ->
	[{Res, pid_to_list(ClientPid)}|T].

disconnect_client(_State, _ClientPid) ->
	initialise_state().
