-module(perms_tests).
-include_lib("eunit/include/eunit.hrl").

perms_test_() ->
    {"It must generate the corrent permutations", 
     ?_assertEqual([
		    [[[1,2,3]],[[1,3,2]]],
		    [[[2,3,1]],[[2,1,3]]],
		    [[[3,2,1]],[[3,1,2]]]
		   ], perms:permute([1,2,3]))}.
