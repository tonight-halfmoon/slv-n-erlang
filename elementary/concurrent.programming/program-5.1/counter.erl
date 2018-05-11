-module(counter).
-export([start/0,loop/1]).

start() ->
    spawn(counter, loop, [0]).

loop(Val) ->
    receive
	increment ->
	    loop(Val + 1)

    end.

%% This example demonstrates many basic concepts:

%% * A new counter process is started by each call to counter:start/0. Each process evaluates the function call counter:loop(0).

%% * A recursive function to generate a perpetual process which is suspended when waiting for input. loop is a tail recursive function which ensures that a counter process will evaluate in constant space.

%% * Selective message reception. in this case the message increment.

%% There are, however, many deficiencies in this example. For example, 
%% * There is no way to acees the value of the counter in each process as the data local to a process can only be accessed by the process itself.

%% * The message protocol is explicit. Other processes explicitly send increment messages to each counter.


