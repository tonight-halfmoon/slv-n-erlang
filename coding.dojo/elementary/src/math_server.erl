-module(math_server).
-export([start/0, loop/1]).

start() ->
    spawn(?MODULE, loop, [fun geometry:areas/1]).

loop(F) ->
    receive
	{request, From, Query} ->
	    From ! {self(), ok, F(Query)},
	    loop(F)
    end.
