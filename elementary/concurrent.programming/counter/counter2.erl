-module(counter2).
-export([start/0, read/1]).

start() ->
    spawn(fun() -> counter(0) end).

read(Pid) ->
    Pid ! {self(), read},
    receive
	{Pid, N} ->
	    io:format("I got N~w~n", [N]),
	    N
    end.

counter(N) ->
    receive
	bump ->
	    counter(N+1);
	{From, read} ->
	    From ! {self(), N},
	    counter(N);
	stop ->
	    true
    end.
	    
