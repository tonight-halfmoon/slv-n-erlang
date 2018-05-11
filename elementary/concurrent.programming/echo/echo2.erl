-module(echo2).
-export([start/0, loop/0]).

start() ->
    spawn(?MODULE, loop, []).

loop() ->
    receive
	L when is_list(L) ->
	    io:format("Received a list: ~p~n", [L]),
	    loop();
	{Pid, L} when is_list(L), is_pid(Pid) ->
	    io:format("Received a tuple of a Pid: ~p and a list: ~p. ~nI will reverse the list and sent it back to you~n", [Pid, L]),
	    R = lists:reverse(L),
	    %Pid ! lists:reverse(L),
	    Pid ! R,
	    loop();	    
	S ->
	    io:format("Received: ~p~n", [S]),
	    loop()
	end.
