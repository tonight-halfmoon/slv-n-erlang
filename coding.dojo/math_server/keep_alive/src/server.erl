-module(server).
-export([start/0, sum_areas/2, stop/1,
	 init/1,
	 on_exit/1]).

-include("server.hrl").

start() ->
    Pid = spawn(?MODULE, init, [fun geometry:areas/1]),
    register(?math_server, Pid),
    spawn(?MODULE, on_exit, [Pid]),
    {ok, Pid}.

sum_areas(Shapes, Pid) ->
    Pid ! {request, self(), Shapes},
    receive
	{reply, Pid, Result} ->
	    Result
    end.

stop(Pid) ->
    Pid ! stop,
    {ok, stopped}.

init(F) ->
    loop(F).

loop(F) ->
    receive
	{request, Client, Shapes} ->
	    Result = eval(F, Shapes),
	    Client ! {reply, self(), Result},
	    loop(F);
	stop ->
	    exit(normal);
	crash ->
	    exit(intin_crash);
	_M ->
	    loop(F)
    end.

eval(F, Shapes) ->
    case catch F(Shapes) of
	{'EXIT', Why} ->
	    {error, Why};
	Sum ->
	    {ok, Sum}
    end.

on_exit(Pid) ->
    process_flag(trap_exit, true),
    link(Pid),
    on_exit(Pid, fun keep_alive/1).

on_exit(Pid, F) ->
    receive
	{'EXIT', Pid, Why} ->
	    F(Why),
	    exit(normal);
	M ->
	    M
    end.

keep_alive(intin_crash) ->
    start();
keep_alive(_Why) ->
    ok.
