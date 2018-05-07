-module(server).
-export([start/0, sum_areas/2, stop/1,
	 loop/2,
	 keep_state/3]).

start() ->
    Pid = spawn(?MODULE, loop, [fun geometry:areas/1, {}]),
    {ok, Pid}.

sum_areas(Shapes, ServerPid) ->
    ServerPid ! {request, self(), {sum_areas, Shapes}},
    receive
	{reply, ServerPid, Result} ->
	    Result
    end.

stop(ServerPid) ->
    case not is_process_alive(ServerPid) of
	true ->
	    {error, already_stopped};
	false ->
	    spawn(?MODULE, keep_state, [self(), ServerPid, {}]),
	    receive
		{ok, stopped, State} ->
		    {ok, stopped, State}
	    end
    end.

keep_state(ClientPid, Pid, State) ->
    process_flag(trap_exit, true),
    link(Pid),
    Pid ! {stop, self()},
    loop_keep_state(ClientPid, Pid, State).

loop_keep_state(ClientPid, Pid, State) ->
    receive
	{'EXIT', Pid, _Why} ->
	    ClientPid ! {ok, stopped, State};
	{server_state, Pid, NewState} ->
	   loop_keep_state(ClientPid, Pid, NewState)
    end.

loop(F, State) ->
    receive
	{request, Client, {sum_areas, Shapes}} ->
	    Result = eval(F, Shapes),
	    Client ! {reply, self(), Result},
	    NewState = {Result, Shapes},
	    loop(F, NewState);
	{stop, From} ->
	    From ! {server_state, self(), State},
	    exit(normal)
    end.

eval(F, Shapes) ->
    case catch F(Shapes) of
	{'EXIT', Why} ->
	    {error, Why};
	Sum ->
	    {ok, Sum}
    end.
