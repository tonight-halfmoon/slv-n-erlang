-module(perms).
-export([permute/1]).
-import(dist, [dist/2]).
-import(swap, [swap/1]).
-include_lib("eunit/include/eunit.hrl").

%%% Permutations with Backtracking with divide and conquer algorithm implementation

permute([X,Y]) ->
    [[X,Y],[Y,X]];
permute(L) ->
    [[dist([HS], Prmtd) || Prmtd <- permute(SL)] || [HS|SL] <- swap(L)].


perms_test_() ->
    {"It must generate the corrent permutations", 
     ?_assertEqual([
		    [[[1,2,3]],[[1,3,2]]],
		    [[[2,3,1]],[[2,1,3]]],
		    [[[3,2,1]],[[3,1,2]]]
		   ], permute([1,2,3]))}.
