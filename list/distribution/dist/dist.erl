-module(dist).
-include_lib("eunit/include/eunit.hrl").
-export([dist/2]).

dist_test_() ->
    {"Must distribute [1] to [2,3] and [3,2] and that yields in '[1,3,2],[1,2,3]'", ?_assertEqual([[1,3,2], [1,2,3]], dist([1],[[2,3],[3,2]]))}.

dist_deep_test_() ->
    {"Must distribute [1] to [[[2,3], [9,8]], [3,2]] and that yields in '[1,3,2], [1,9,8], [1,2,3]'", ?_assertEqual([[1,3,2], [1,2,3], [1,9,8]], dist([1],[[[2,3],[9,8]],[3,2]]))}.

dist_2deeplists_test_() ->
    {"Must distribute '[[1,9], [9,1]]' to '[[2,3], [5,6]]' and that yields in '[1,9,2,3], [1,9,5,6], [9,1,2,3], [9,1,5,6]'",
     ?_assertEqual([[1,9,2,3],[1,9,5,6],[9,1,2,3],[9,1,5,6]], 
		   dist([[1,9],[9,1]], [[2,3],[5,6]]))}.


dist_ListOfA_and_deeplists_test_() ->
{"Must distribute [7] to a List of Lists and first match",
 ?_assertMatch([X|_] when X == [7,3,1,2,6,5,4], dist([7],
[[2,3,1,4,6,5],[2,3,1,5,4,6],[2,3,1,6,5,4],[1,2,3,4,6,5],[1,2,3,5,4,6],[1,2,3,6,5,4],[3,1,2,4,6,5],[3,1,2,5,4,6],[3,1,2,6,5,4]]))}.

dist(X, ListOfLists) ->
    case is_list(X) of
	true ->
	    dist(X, ListOfLists, []);
	false ->
	    dist([X], ListOfLists, [])
    end.

dist([], _, U) ->
    U;
dist(_Xs, [], U) ->
    U; 
dist([H|T], ListOfLists, U) when is_list(H) ->
    dist(T, ListOfLists, concat(dist(H, ListOfLists), U));
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
