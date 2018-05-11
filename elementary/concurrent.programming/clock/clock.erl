-module(clock).
-export([start/1, start/2, stop/0, real_time/0]).

start(Time) ->
    start(Time,  fun clock:real_time/0).

start(Time, Fun) ->
    register(clock, spawn(fun() -> tick(Time, Fun) end)).

stop() ->
    clock ! stop.

tick(Time, Fun) ->
    receive
	stop ->
	    io:format("Someone has stopped me!~n"),
	    void
    after Time ->
	    Fun(),
	    tick(Time, Fun)
   end.


    
real_time() ->
    io:format("Tick ~p~n", [erlang:now()]).
