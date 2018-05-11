%%% 9th Feb 16; Jeddah, SA %%%
-module(binarysearch).
-export([bs/2, mid/1]).
-include_lib("eunit/include/eunit.hrl").

mid_even_list_test()->
    EV = {[1,2,3], [45,5,6]},
    L = [1,2,3,45,5,6],
    ?assertEqual(EV, mid(L)).
mid_odd_list_test()->
    EV = {[1,2,3], [45,5,6,34]},
    L = [1,2,3,45,5,6,34],
    ?assertEqual(EV, mid(L)).

mid_odd_list_small_test()->
    EV = {[1], [2,3]},
    L = [1,2,3],
    ?assertEqual(EV, mid(L)).
mid_even_list_tiny_test()->
    EV = {[2],[5]},
    L = [2,5],
    ?assertEqual(EV, mid(L)).
mid_odd_list_big_test()->
    EV = {[1,2,3,45,5,6,34,55,66,77], [88,99,90,123,12345,123,12,193,532,1235]},
    L = [1,2,3,45,5,6,34,55,66,77,88,99,90,123,12345,123,12,193,532,1235],
    ?assertEqual(EV, mid(L)).

bs_test()->
    L = [1,2,3,4,5,6,7],
    ?assertEqual(found,bs(7,L)).
bs_not_found_test()->
    L = [1,2,3,4,5,6,7],
    ?assertEqual(not_found, bs(8,L)).
bs_not_sorted_list_test()->
    L = [5,6,3,2,9,7,45,32,12,102,83,65,34,98,34,90,87,76,54,42],
    ?assertEqual(found, bs(102,lists:sort(L))).
bs_empty_list_test()->
    ?assertEqual(not_found, bs(7,[])).

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
