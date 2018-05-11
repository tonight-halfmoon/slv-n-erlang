-module(queue).
-export([main/0]).

-define(STACK1, []).
-define(STACK2, 3).

%% implement a Queue using two stacks
main()->
    {ok, [D]} = io:fread("Prompt> ", "~d"),
    io:fwrite("D: ~w~n", [D]),
    io:fwrite("STACK1: ~w~n", [?STACK1]),
    io:fwrite("STACK2: ~w~n", [?STACK2]),

    push(D, ?STACK1),
    io:fwrite("STACK1: ~w~n", [?STACK1]),

    true.

push(X, Stack) ->
    Stack++[X].
