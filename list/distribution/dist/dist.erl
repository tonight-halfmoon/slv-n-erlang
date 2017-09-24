-module(dist).
-include_lib("eunit/include/eunit.hrl").
-export([dist/2]).

dist_test_() ->
    {"Must distribute [1] to [2,3] and [3,2] and that yields in '[1,3,2],[1,2,3]'", ?_assertEqual([[1,3,2], [1,2,3]], dist([1],[[2,3],[3,2]]))}.

dist(List, ListOfLists) ->
    dist(List, ListOfLists, []).

dist(_Xs, [], U) ->
    U; 
dist(Xs, [[H|T]|OtherLists], U) when is_list(H) ->
    dist(Xs, OtherLists, [dist(Xs, [H|T])|U]);
dist(Xs, [L|OtherLists], U) when is_list(L) ->
    dist(Xs, OtherLists, [lists:append(Xs, L)|U]);
dist(Xs, L, U) ->
    dist(Xs, [], [lists:append(Xs, L)|U]).
