-module(mailbox).

-export([start/0, loop/0]).


start() ->
    spawn(?MODULE, loop, []).


loop() ->
    receive
	{a, H} ->
	    io:format("Received {a, ~p}~n", [H]),
      loop();
	{b, H} ->
	    io:format("Received {b, ~p}~n", [H]),
	    loop();
	stop ->
	    io:format("Process ~p is termintated.~n", [self()]),
	    void
    after 10000 ->
	    exit(timeout)
    end.
