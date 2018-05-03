-module(server).
-export([start/0, sum_areas/2, stop/1,
	 loop/1]).

start() ->
    spawn(?MODULE, loop, [fun geometry:areas/1]).

sum_areas(Shapes, ServerPid) ->
    ServerPid ! {request, self(), Shapes},
    receive
	{reply, ServerPid, Result} ->
	    Result
    end.

stop(ServerPid) ->
    ServerPid ! stop.

loop(F) ->
    receive
	{request, Client, Shapes} ->
	    Result = eval(F, Shapes),
	    Client ! {reply, self(), Result},
	    loop(F);
	stop ->
	    ok
    end.

eval(F, Shapes) ->
    case catch F(Shapes) of
	{'EXIT', Why} ->
	    {error, Why};
	Sum ->
	    {ok, Sum}
    end.
