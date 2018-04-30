-module(math_server).
-export([start/0, loop/1, cast/2]).

start() ->
    spawn(?MODULE, loop, [fun geometry:areas/1]).

cast(MathServerPid, Message) ->
    MathServerPid ! Message,
    ok.

loop(F) ->
    receive
	{print, Shapes} ->
	    receive
	    after 5000 ->
		    ok
	    end,
	    io:format("Areas ~p~n", [F(Shapes)]),
	    loop(F)
    end.
