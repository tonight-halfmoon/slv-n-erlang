-module(perms_altv).
-export([permute/1, permute_alt3/1]).
-import(dist, [dist/2]).
-import(swap, [swap/1]).
-include_lib("eunit/include/eunit.hrl").

%%% Permutations - implementation with Backtracking and, divide and conquer algorithm

permute([X,Y]) ->
    [[X,Y],[Y,X]];
permute(L) ->
    lists:map(fun([XS|SL]) -> [dist([XS], Prmtd) || Prmtd <- permute(SL)] end, swap(L)).
   

permute_alt3([X,Y]) ->
    [[X,Y],[Y,X]];
permute_alt3(L) ->
 lists:map(fun([XS|SL]) -> lists:map(fun(Prmtd) -> dist([XS], Prmtd) end, permute_alt3(SL)) end, swap(L)).



perms_test_() ->
    {"It must generate the corrent permutations", 
     ?_assertEqual([
		    [[[1,2,3]],[[1,3,2]]],
		    [[[2,3,1]],[[2,1,3]]],
		    [[[3,2,1]],[[3,1,2]]]
		   ], permute([1,2,3]))}.
