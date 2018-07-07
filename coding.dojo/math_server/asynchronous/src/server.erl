-module(server).
-export([start/0, stop/1, sum_areas/2, async_sum_areas/2]).
-export([loop/1, cast/3]).

start() ->
    Pid = spawn(?MODULE, loop, [fun geometry:areas/1]),
    {ok, Pid}.

sum_areas(Shapes, ServerPid) ->
    ServerPid ! {self(), {sum_areas, Shapes}},
    receive
	M ->
	    M
    end.

async_sum_areas(Shapes, ServerPid) ->
    spawn(?MODULE, cast, [self(), {sum_areas, Shapes}, ServerPid]),
    {ok, noreply}.

stop(Pid) ->
    Pid ! stop,
    {ok, stopped}.

loop(Callback) ->
    receive
	{From, {sum_areas, Shapes}} ->
	    Result = eval(Callback, Shapes),
	    From ! Result,
	    loop(Callback);
	stop ->
	    ok
    end.

eval(Fun, Args) ->
    case catch Fun(Args) of
	{'EXIT', Why} ->
	    {error, Why};
	Output ->
	    {ok, Output}
    end.

cast(ClientPid, Request, ServerPid) ->
    ServerPid ! {self(), Request},
    receive
	M ->
	    ClientPid ! M,
	    exit(normal)
    end.
