-module(math_server).
-export([start/0, loop/1]).

start() ->
    register(?MODULE, spawn(?MODULE, loop, [fun geometry:areas/1])).

loop(F) ->
    receive
	{request, From, Query} ->
	    From ! {?MODULE, ok, F(Query)},
	    loop(F)
    after 1500000 ->
	    io:format("Timeout. Server shutdown.~n", []),
	    exit(timeout)
	end.
