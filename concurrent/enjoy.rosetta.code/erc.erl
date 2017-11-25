-module(erc).
-export([start/0]).

%%% this is how you enjoy concurrency with rosetta code!
%%% This sample simulates synchronous behaviour of multiple processes

%%% Sample output: 
%%% 36> erc:start().
%%% [<0.178.0>,<0.179.0>,<0.180.0>]
%%% 'Code'
%%% 'Enjoy'
%%% 'Rosetta'
%%% 37> 

start() ->
    [spawn(fun() -> say(self(), X, rand:uniform(5500)) end) || X <- ['This', 'how', 'Enjoy', 'Rosetta', 'Code', 'is', 'You']].

say(Pid, Word, N) ->
    Pid ! Word,
    wait(N).

wait(N) ->
    receive
    after
	N ->
	    receive
		Word ->
		    io:format("~p~n", [Word])
	    end
    end.
