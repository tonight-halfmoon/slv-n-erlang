-module(echo).
-export([start/0, loop/0]).

start() ->
    spawn(echo, loop, []).

loop() ->
    receive 
	{From, Message} ->
	    From ! Message,
	    loop();
	Message ->
	    io:format("Received Message: ~p~n", [Message]),
	    loop()
    end.
