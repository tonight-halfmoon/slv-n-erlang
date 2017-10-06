-module(string_o_permute).
-include_lib("eunit/include/eunit.hrl").
-export([main/0]).
-import(swap, [swap_adjacent/1]).

main() ->
    {ok, [T]} = io:fread("", "~d"),
    PL = run_test_cases_on(T, []),
    display(PL),
    true.

run_test_cases_on(0, PL) ->
    PL;
run_test_cases_on(Countdown, PL) ->
    {ok, [L]} = io:fread("", "~ts"),
    run_test_cases_on(Countdown-1, lists:append(PL, [swap:swap_adjacent(L)])).

display([]) -> 
    ok;
display([H|T]) ->
    display(H, T).

display(X, []) ->
    io:fwrite("~s~n", [X]);
display(X, [H|T]) ->
    io:fwrite("~s~n", [X]),
    display(H, T).

string_o_premute_test_() ->
    {"swap adjacent for 'abcdpqrs' must yield in 'badcqpsr'", ?_assertEqual("badcqpsr", swap:swap_adjacent("abcdpqrs")) }.

string_o_premute_za_test_() ->
    {"swap adjacent for 'za' must yield in 'az'", ?_assertEqual("az", swap:swap_adjacent("za")) }.

