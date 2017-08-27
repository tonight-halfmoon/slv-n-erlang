%%% 9th Feb 16; Jeddah, SA %%%
-module(bs).
-include_lib("eunit/include/eunit.hrl").
-export([bs/2, mid/1]).

mid_even_list_test()->
    Expect = {[1,2,3], [45,5,6]},
    Linput = [1,2,3,45,5,6],
    ?assertEqual(Expect, mid(Linput)).

mid_odd_list_test()->
    Expect = {[1,2,3], [45,5,6,34]},
    Linput = [1,2,3,45,5,6,34],
    ?assertEqual(Expect, mid(Linput)).

mid_odd_list_small_test()->
    Expect = {[1], [2,3]},
    Linput = [1,2,3],
    ?assertEqual(Expect, mid(Linput)).

mid_even_list_tiny_test()->
    Expect = {[2], [5]},
    Linput = [2,5],
    ?assertEqual(Expect, mid(Linput)).

mid_odd_list_big_test()->
    Expect = {[1,2,3,45,5,6,34,55,66,77], [88,99,90,123,12345,123,12,193,532,1235]},
    Linput = [1,2,3,45,5,6,34,55,66,77,88,99,90,123,12345,123,12,193,532,1235],
    ?assertEqual(Expect, mid(Linput)).

bs_test() ->
    ?assertEqual(found, bs(7,[1,2,3,4,5,6,7])).

bs_must_find_1st_test() ->
    ?assertEqual(found, bs(1,[1,2,3,4,5,6,7])).

bs_not_found_test() ->
    ?assertEqual(not_found, bs(8,[1,2,3,4,5,6,7])).

bs_list_must_be_sorted_then_102_must_be_found_test() ->
    ?assertEqual(found, bs(102, lists:sort([5,6,3,2,9,7,45,32,12,102,83,65,34,98,34,90,87,76,54,42]))).

bs_must_not_be_found_in_empty_list_test() ->
    ?assertEqual(not_found, bs(7, [])).

mid([])->
    [];
mid([_]) ->
    [];
mid(L)->
    mid(L, []).

mid(L, L2) when length(L) =:= length(L2) ; length(L) =:= length(L2) + 1 ->
    {L2, L};
mid([H|T], L) ->
    mid(T, L ++ [H]).

bs(X, [X|_]) ->
    found;
bs(_, []) ->
    not_found;
bs(X, {_, [H|T]}) when X >= H ->
    bs(X, [H|T]);
bs(X, {[H|T], _}) when X < H ->
    bs(X, [H|T]);
bs(X, L) ->
    bs(X, mid(L)).
