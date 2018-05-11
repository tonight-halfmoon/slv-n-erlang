-module(counter3).
-export([start/0, bump/1, read/1]).

start () ->
    spawn(fun() -> counter(0) end).

bump(Pid) ->
    Pid ! bump.


read(Pid) ->
    Pid ! {self(),read},
    receive 
	{Pid, N} ->
	    N
    after 1000 ->
	   noreply
    end.

counter(N) ->
    receive 
	bump ->
	    counter(N+1);
	 {From, read}->
	    From ! {self(), N},
	    counter(N);
	stop ->
	    true
     end.
