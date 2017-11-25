-module(sort).
-export([sort/1]).
-include_lib("eunit/include/eunit.hrl").


sort_letters_test_() ->
    {"", ?_assertEqual(["a", "b", "c"], sort([ "c", "b", "a"]))}.

sort_numbers_test_() ->
    {"", ?_assertEqual([1, 4, 7], sort([ 4, 1, 7]))}.

sort(L) ->
    sort(L, fun asc/2).

sort(L, Fun) ->
    lists:map(fun(X) -> lists:map(fun(Y) -> Fun(X, Y) end, L -- [X]) end, L).

asc(X, Y) when X =< Y ->
    X;
asc(_, Y) ->
    Y.
