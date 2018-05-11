-module(timer5000).
-export([timer/0, timer/1]).


timer() ->
    spawn(timer5000, timer, [self()]).


timer(Pid) ->
    receive
    after
	0 ->
	    io:format("timer spanned 5000 ms ~n", []),
	    Pid ! timeout,
	    io:format("timer exited ~n", [])

    end.
