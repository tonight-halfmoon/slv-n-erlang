-module(echo).
-export([rpc/1, loop/0]).

rpc(Msg) ->
    echo ! Msg,
    receive
	Msg ->
	    io:format("RPC received ~p~n", [Msg])
	end.

loop() ->
    receive
	{From, Msg} ->
	    io:format("~p received Msg ~p~n", [?MODULE, Msg]),
	    From ! {self(), Msg},
	    loop();
	stop ->
	    io:format("~p received ~p message~n", [?MODULE, stop]),
	    exit(normal)
	end.
