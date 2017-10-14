-module(perms_altv).
-export([permute/1, permute_alt3/1]).
-import(dist, [dist/2]).
-import(swap, [swap/1]).
-include_lib("eunit/include/eunit.hrl").

%%% Permutations - implementation with Backtracking and, divide and conquer algorithm

permute([X,Y]) ->
    [[X,Y],[Y,X]];
permute(L) ->
    lists:map(fun([H|T]) -> [dist(H, P) || P <- permute(T)] end, swap(L)).
   

permute_alt3([X,Y]) ->
    [[X,Y],[Y,X]];
permute_alt3(L) ->
 lists:map(fun([H|T]) -> lists:map(fun(P) -> dist(H, P) end, permute_alt3(T)) end, swap(L)).

perms_test_() ->
    {"It must generate the corrent permutations", 
     ?_assertEqual([
		    [[1,2,3],[1,3,2]],
		    [[2,1,3],[2,3,1]],
		    [[3,1,2],[3,2,1]]
		    ], permute([1,2,3]))}.


perms_7_test_() ->
    {"Permute '[1..7], 7' must halt", 
    ?_assertMatch(L when length(L) == 7, permute(lists:seq(1, 7)))}.

perms_2500_test_() ->
    {"Permute '[1..2500], 2500' must halt", 
    ?_assertMatch(L when length(L) == 2500, permute(lists:seq(1, 2500)))}.

perms_alt3_2500_test_() ->
    {"Permute alt3 '[1..2500], 2500' must halt", 
    ?_assertMatch(L when length(L) == 2500, permute_alt3(lists:seq(1, 2500)))}.
