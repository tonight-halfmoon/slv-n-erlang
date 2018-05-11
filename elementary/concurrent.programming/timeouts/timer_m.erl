-module(timer_m).
-export([timeout/2, cancel/1,timer/3, alarm/1]).

timeout(Time, Alarm) ->
    spawn(timer_m, timer, [self(), Time, Alarm]).

cancel(Timer) ->
    Timer ! {self(), cancel}.

alarm(Timer) ->
     Timer ! alarm.

timer(Pid, Time, Alarm) ->
    receive
	{Pid,cancel} ->
	    void;
         al ->
	    io:format("Alarm!!!!")	       
    after Time ->
	    Pid ! Alarm
end.
