-module(countpairs).
-export([main/0, countpairs_in/1]).
-import(countoccs, [coccs/2]).
-import(choose, [choose/2]).
-include_lib("eunit/include/eunit.hrl"). 

%%% Problem Statement
%%% Count the total number of pairs of indices (i, j) where Ai = Aj and i =/= j.

countpairs_in([])->
    0;
countpairs_in([H|T]) ->
    countpairs_in(H, [H|T], 0).

countpairs_in(_Hi, [] , AAcc) ->
    AAcc;
countpairs_in(Hi, L, AAcc) ->
    case countoccs:coccs(Hi, L) of
	{XOccs, []} ->
	    AAcc +  choose:choose(XOccs, 2)  * 2;
	{XOccs, [NewHj|NewT]} ->
	    countpairs_in(NewHj, [NewHj|NewT], AAcc + choose:choose(XOccs, 2) * 2) 
    end.

%%% How 2 use the main function
%%% 14> solution2:main().
%%% 2
%%% 3
%%% 1 2 3
%%% 2
%%% 1 1
%%% 0
%%% 2
%%% true
%%% 15> 
main() ->
    {ok, [T]} = io:fread("", "~d"),
    PL = run_test_cases_on(T, []),
    display(PL),
    true.

run_test_cases_on(0, PL) ->
    PL;
run_test_cases_on(Countdown, PL) ->
    {ok, [List_size]} = io:fread("", "~d"),
    {ok, L} = io:fread("", string:join(replicate(List_size, "~d"), " ")),
    run_test_cases_on(Countdown-1, lists:append(PL, [countpairs_in(L)])).

display([]) -> 
    ok;
display([H|T]) ->
    display(H, T).

display(X, []) ->
    io:fwrite("~w~n", [X]);
display(X, [H|T]) ->
    io:fwrite("~w~n", [X]),
    display(H, T).

replicate(Bndry, _) when Bndry =< 0 ->
    [];
replicate(Bndry, O) -> [O|replicate(Bndry-1, O)].


sherlok_pairs_1_1_1_test_() ->
    {"Total pairs in '[1,1,1]' is '6'", ?_assertEqual(6, countpairs_in([1,1,1]))}.

sherlok_pairs_1_1_1_1_test_() ->
    {"Total pairs in '[1,1,1,1]' is '12'", ?_assertEqual(12, countpairs_in([1,1,1,1]))}.

sherlok_pairs_test_() ->
    {"Total pairs in '[1,1,2]' is '2'", ?_assertEqual(2, countpairs_in([1,1,2]))}.

sherlok_pairs_1_2_1_test_() ->
    {"Total pairs in '[1,2,1]' is '2'", ?_assertEqual(2, countpairs_in([1,2,1]))}.

sherlok_pairs_empty_test_() ->
    {"Total pairs in '[]' is '0'", ?_assertEqual(0, countpairs_in([]))}.
