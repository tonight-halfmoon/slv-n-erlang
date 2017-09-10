-module(countoccs).
-export([coccs/2]).
-include_lib("eunit/include/eunit.hrl").
-include("./include/testcase_input02_04.hrl").

coccs(X, L) ->
    coccs(X, L, 0, []).

coccs(_, [], Occs, Rest) ->
    {Occs, Rest};
coccs(X, [X|T], Occs, Rest) ->
    coccs(X, T, Occs + 1, Rest);
coccs(X, [H|T], Occs, Rest) ->
    coccs(X, T, Occs, [H|Rest]).

%%% For performance reason, the returned list is in reverse order.
%%% Using lists:append/2, is time-consuming for very large lists> 41000 elements each of having 10^5, 10^6 integer value. 

coccs_test() ->
    ?assertEqual({2, [c,c,b]}, coccs(a, [a,b,c,c,a])).

count_empty_lst_test() ->
    ?assertEqual({0, []}, coccs(1, [])).

coccs_2_2_test() ->
    ?assertEqual({2, [3]}, coccs(2, [2,3,2])).

coccs_testcase_02_02_test_() ->
    {"'1000000' occurred '100000' in testcase_02_04 input list", ?_assertEqual({100000, []}, coccs(1000000, ?testcase_input02_04()))}.
