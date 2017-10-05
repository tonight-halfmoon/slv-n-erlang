-module(countpairs).
-export([main/0, countpairs/1]).
-import(countoccs, [coccs/2]).
-include_lib("eunit/include/eunit.hrl"). 

%%% Problem Statement
%%% Count the total number of pairs of indices (i, j) where Ai = Aj and i =/= j.
%%% Since Choose(N,2) is equivalent to N*(N -1). Then, no need to compute with a time-consuming choose implementation.
%%% Replaced 'choose:choose(V, 2) * 2' , or 'choose:choose(V, 2) bsl 1' with V*(V-1). 

countpairs([])->
    0;
countpairs([H|T]) ->
    countpairs(H, [H|T], 0).

countpairs(_Hi, [] , AAcc) ->
    AAcc;
countpairs(Hi, L, AAcc) ->
    case countoccs:coccs(Hi, L) of
	{XOccs, []} ->
	    AAcc +  XOccs * (XOccs - 1);
	{XOccs, [NewHj|NewT]} ->
	    countpairs(NewHj, [NewHj|NewT], AAcc + XOccs * (XOccs - 1)) 
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
    run_test_cases_on(Countdown-1, lists:append(PL, [countpairs(L)])).

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
    {"Total pairs in '[1,1,1]' is '6'", ?_assertEqual(6, countpairs([1,1,1]))}.

sherlok_pairs_1_1_1_1_test_() ->
    {"Total pairs in '[1,1,1,1]' is '12'", ?_assertEqual(12, countpairs([1,1,1,1]))}.

sherlok_pairs_test_() ->
    {"Total pairs in '[1,1,2]' is '2'", ?_assertEqual(2, countpairs([1,1,2]))}.

sherlok_pairs_1_2_1_test_() ->
    {"Total pairs in '[1,2,1]' is '2'", ?_assertEqual(2, countpairs([1,2,1]))}.

sherlok_pairs_empty_test_() ->
    {"Total pairs in '[]' is '0'", ?_assertEqual(0, countpairs([]))}.
