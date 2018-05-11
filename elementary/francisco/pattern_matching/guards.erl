-module(guards).
-export([test/1]).

test({A}) when is_atom(A) ->
    io:format("A is 0 ~p~n", [A]);
test({A}) when A == 1 ->
    ok.
