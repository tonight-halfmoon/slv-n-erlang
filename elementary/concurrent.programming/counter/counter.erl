-module(counter).
-export([start/0,loop/1,increment/1,value/1,stop/1, other/0]).

%% First the interface functions.
start() ->
    spawn(counter, loop, [0]).

%% Hide the protocol.
increment(Counter) ->
    Counter ! increment.

value(Counter) ->
    io:format("self(): ~p~n", [self()]),
    io:format("Counter Pid: ~p~n", [Counter]),
    Counter ! {self(), value}, %%% self() is the sender's Pid
    receive
	{Counter, Value} ->
	    Value
    end.
other() ->
    hehehe.

stop(Counter) ->
    Counter ! stop.

%% The counter loop.
loop(Val) ->
    receive 
	increment ->
	    loop(Val + 1);
	{From, value} ->
	    From ! {self(), Val},
	    loop(Val);
	stop ->          % No recursive call here
	    true;
	Other ->         % All other messages; Receive them to get them out of the mailbox
	    loop(Val)
end.
