-module(solution2).
-export([main/0]).
-include_lib("eunit/include/eunit.hrl").

main()->
    {ok, [T]} = io:fread("", "~d"),
    PL = run_test_cases_on(T, []),
    display(PL),
    true.

run_test_cases_on(0, PL)->
    PL;
run_test_cases_on(Countdown, PL) ->
    %{ok, [N]} = io:fread("", "~d"),
    %{ok, L} = io:fread("", string:join(replicate(N, "~d"), " ")),
    L = lists:seq(1, 100000),
    run_test_cases_on(Countdown-1, lists:append(PL, [count_pairs(L)])).

count_pairs([])->
    0;
count_pairs(L) ->
    count_pairs(L, 0).
count_pairs([H|T], Acc)->
    L2 = lists:delete(H, [H|T]),
    count_pairs(T, count_eq(H,L2) + Acc);
count_pairs([], Acc) ->
    Acc *2.

count_eq(X, L)->
    count_eq(X, L, 0).
count_eq(X, [H|T], Acc) when X =:= H ->
    count_eq(X, T, Acc+1);
count_eq(X, [_|T], Acc) ->
    count_eq(X, T, Acc);
count_eq(_, [], Acc) ->
    Acc.

display([]) -> ok;
display(X) when is_integer(X) ->
    io:fwrite("~w~n",[X]);
display(PL) when is_list(PL) ->
    [H|T] = PL,
    display(H),
    display(T).


replicate(Bndry, _) when Bndry =< 0 ->
    [];
replicate(Bndry, O) -> [O|replicate(Bndry-1, O)].
