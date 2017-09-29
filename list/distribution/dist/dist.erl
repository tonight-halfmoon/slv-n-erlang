-module(dist).
-include_lib("eunit/include/eunit.hrl").
-export([dist/2]).

dist_test_() ->
    {"Must distribute [1] to [2,3] and [3,2] and that yields in '[1,3,2],[1,2,3]'", ?_assertEqual([[1,3,2], [1,2,3]], dist([1],[[2,3],[3,2]]))}.

dist_deep_test_() ->
    {"Must distribute [1] to [[[2,3], [9,8]], [3,2]] and that yields in '[1,3,2], [1,9,8], [1,2,3]'", ?_assertEqual([[1,3,2], [1,2,3], [1,9,8]], dist([1],[[[2,3],[9,8]],[3,2]]))}.


dist(X, ListOfLists) ->
    case is_list(X) of
	true ->
	    dist(X, ListOfLists, []);
	false ->
	    dist([X], ListOfLists, [])
    end.

dist(_Xs, [], U) ->
    U; 
dist(Xs, [[H|T]|R], U) when is_list(H) ->
    dist(Xs, R,  concat(dist(Xs, [H|T]),U));
dist(Xs, [L|[]], []) when is_list(L) ->
    concat(Xs, L);
dist(Xs, [L|R], U) when is_list(L) ->
    dist(Xs, R, [concat(Xs, L)|U]);
dist(Xs, L, []) ->
    concat(Xs, L).

concat([], L) ->
    L;
concat([H|T], L) ->
    concat(T, [H|L]).
