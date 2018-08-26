-module(fulqualif).

-export([start/0, api/1, a/1]).

%% Callback
-export([init/0]).

-define(proc_name, 'process-and-fully-qualified-call').

start() ->
    register(?proc_name, spawn(?MODULE, init, [])).

init() ->
    loop().

loop() ->
    receive
	{From, N} ->
	    From ! fulqualif:a(N)
    end,
    loop().

api(N) ->
    ?proc_name ! {self(), N},
    receive
	Reply ->
	    Reply
    end.

a(N) ->
    N.
