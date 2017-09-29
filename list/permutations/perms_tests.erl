-module(perms_tests).
-include_lib("eunit/include/eunit.hrl").

perms_test_() ->
    {"It must generate the corrent permutations", 
     ?_assertEqual([[[1,2,3],[1,3,2]],[[2,1,3],[2,3,1]],[[3,1,2],[3,2,1]]], perms:permute([1,2,3]))}.


perms_1_2_3_4_5_test_() ->
    ?_assertEqual([[[[1,2,5,4,3],
		     [1,2,5,3,4],
		     [1,2,4,5,3],
		     [1,2,4,3,5],
		     [1,2,3,5,4],
		     [1,2,3,4,5]],
		    [[1,3,5,4,2],
		     [1,3,5,2,4],
		     [1,3,4,5,2],
		     [1,3,4,2,5],
		     [1,3,2,5,4],
		     [1,3,2,4,5]],
		    [[1,4,5,3,2],
		     [1,4,5,2,3],
		     [1,4,3,5,2],
		     [1,4,3,2,5],
		     [1,4,2,5,3],
		     [1,4,2,3,5]],
		    [[1,5,4,3,2],
		     [1,5,4,2,3],
		     [1,5,3,4,2],
		     [1,5,3,2,4],
		     [1,5,2,4,3],
		     [1,5,2,3,4]]],
		   [[[2,1,5,4,3],
		     [2,1,5,3,4],
		     [2,1,4,5,3],
		     [2,1,4,3,5],
		     [2,1,3,5,4],
		     [2,1,3,4,5]],
		    [[2,3,5,4,1],
		     [2,3,5,1,4],
		     [2,3,4,5,1],
		     [2,3,4,1,5],
		     [2,3,1,5,4],
		     [2,3,1,4,5]],
		    [[2,4,5,3,1],
		     [2,4,5,1,3],
		     [2,4,3,5,1],
		     [2,4,3,1,5],
		     [2,4,1,5,3],
		     [2,4,1,3,5]],
		    [[2,5,4,3,1],
		     [2,5,4,1,3],
		     [2,5,3,4,1],
		     [2,5,3,1,4],
		     [2,5,1,4,3],
		     [2,5,1,3,4]]],
		   [[[3,1,5,4,2],
		     [3,1,5,2,4],
		     [3,1,4,5,2],
		     [3,1,4,2,5],
		     [3,1,2,5,4],
		     [3,1,2,4,5]],
		    [[3,2,5,4,1],
		     [3,2,5,1,4],
		     [3,2,4,5,1],
		     [3,2,4,1,5],
		     [3,2,1,5,4],
		     [3,2,1,4,5]],
		    [[3,4,5,2,1],
		     [3,4,5,1,2],
		     [3,4,2,5,1],
		     [3,4,2,1,5],
		     [3,4,1,5,2],
		     [3,4,1,2,5]],
		    [[3,5,4,2,1],
		     [3,5,4,1,2],
		     [3,5,2,4,1],
		     [3,5,2,1,4],
		     [3,5,1,4,2],
		     [3,5,1,2,4]]],
		   [[[4,1,5,3,2],
		     [4,1,5,2,3],
		     [4,1,3,5,2],
		     [4,1,3,2,5],
		     [4,1,2,5,3],
		     [4,1,2,3,5]],
		    [[4,2,5,3,1],
		     [4,2,5,1,3],
		     [4,2,3,5,1],
		     [4,2,3,1,5],
		     [4,2,1,5,3],
		     [4,2,1,3,5]],
		    [[4,3,5,2,1],
		     [4,3,5,1,2],
		     [4,3,2,5,1],
		     [4,3,2,1,5],
		     [4,3,1,5,2],
		     [4,3,1,2,5]],
		    [[4,5,3,2,1],
		     [4,5,3,1,2],
		     [4,5,2,3,1],
		     [4,5,2,1,3],
		     [4,5,1,3,2],
		     [4,5,1,2,3]]],
		   [[[5,1,4,3,2],
		     [5,1,4,2,3],
		     [5,1,3,4,2],
		     [5,1,3,2,4],
		     [5,1,2,4,3],
		     [5,1,2,3,4]],
		    [[5,2,4,3,1],
		     [5,2,4,1,3],
		     [5,2,3,4,1],
		     [5,2,3,1,4],
		     [5,2,1,4,3],
		     [5,2,1,3,4]],
		    [[5,3,4,2,1],
		     [5,3,4,1,2],
		     [5,3,2,4,1],
		     [5,3,2,1,4],
		     [5,3,1,4,2],
		     [5,3,1,2,4]],
		    [[5,4,3,2,1],
		     [5,4,3,1,2],
		     [5,4,2,3,1],
		     [5,4,2,1,3],
		     [5,4,1,3,2],
		     [5,4,1,2,3]]]],
		  perms:permute([1,2,3,4,5])).


perms__test_() ->
    ?_assertEqual([[[[1,9,8,7,6],
		     [1,9,8,6,7],
		     [1,9,7,8,6],
		     [1,9,7,6,8],
		     [1,9,6,8,7],
		     [1,9,6,7,8]],
		    [[1,6,8,7,9],
		     [1,6,8,9,7],
		     [1,6,7,8,9],
		     [1,6,7,9,8],
		     [1,6,9,8,7],
		     [1,6,9,7,8]],
		    [[1,7,8,6,9],
		     [1,7,8,9,6],
		     [1,7,6,8,9],
		     [1,7,6,9,8],
		     [1,7,9,8,6],
		     [1,7,9,6,8]],
		    [[1,8,7,6,9],
		     [1,8,7,9,6],
		     [1,8,6,7,9],
		     [1,8,6,9,7],
		     [1,8,9,7,6],
		     [1,8,9,6,7]]],
		   [[[9,1,8,7,6],
		     [9,1,8,6,7],
		     [9,1,7,8,6],
		     [9,1,7,6,8],
		     [9,1,6,8,7],
		     [9,1,6,7,8]],
		    [[9,6,8,7,1],
		     [9,6,8,1,7],
		     [9,6,7,8,1],
		     [9,6,7,1,8],
		     [9,6,1,8,7],
		     [9,6,1,7,8]],
		    [[9,7,8,6,1],
		     [9,7,8,1,6],
		     [9,7,6,8,1],
		     [9,7,6,1,8],
		     [9,7,1,8,6],
		     [9,7,1,6,8]],
		    [[9,8,7,6,1],
		     [9,8,7,1,6],
		     [9,8,6,7,1],
		     [9,8,6,1,7],
		     [9,8,1,7,6],
		     [9,8,1,6,7]]],
		   [[[6,1,8,7,9],
		     [6,1,8,9,7],
		     [6,1,7,8,9],
		     [6,1,7,9,8],
		     [6,1,9,8,7],
		     [6,1,9,7,8]],
		    [[6,9,8,7,1],
		     [6,9,8,1,7],
		     [6,9,7,8,1],
		     [6,9,7,1,8],
		     [6,9,1,8,7],
		     [6,9,1,7,8]],
		    [[6,7,8,9,1],
		     [6,7,8,1,9],
		     [6,7,9,8,1],
		     [6,7,9,1,8],
		     [6,7,1,8,9],
		     [6,7,1,9,8]],
		    [[6,8,7,9,1],
		     [6,8,7,1,9],
		     [6,8,9,7,1],
		     [6,8,9,1,7],
		     [6,8,1,7,9],
		     [6,8,1,9,7]]],
		   [[[7,1,8,6,9],
		     [7,1,8,9,6],
		     [7,1,6,8,9],
		     [7,1,6,9,8],
		     [7,1,9,8,6],
		     [7,1,9,6,8]],
		    [[7,9,8,6,1],
		     [7,9,8,1,6],
		     [7,9,6,8,1],
		     [7,9,6,1,8],
		     [7,9,1,8,6],
		     [7,9,1,6,8]],
		    [[7,6,8,9,1],
		     [7,6,8,1,9],
		     [7,6,9,8,1],
		     [7,6,9,1,8],
		     [7,6,1,8,9],
		     [7,6,1,9,8]],
		    [[7,8,6,9,1],
		     [7,8,6,1,9],
		     [7,8,9,6,1],
		     [7,8,9,1,6],
		     [7,8,1,6,9],
		     [7,8,1,9,6]]],
		   [[[8,1,7,6,9],
		     [8,1,7,9,6],
		     [8,1,6,7,9],
		     [8,1,6,9,7],
		     [8,1,9,7,6],
		     [8,1,9,6,7]],
		    [[8,9,7,6,1],
		     [8,9,7,1,6],
		     [8,9,6,7,1],
		     [8,9,6,1,7],
		     [8,9,1,7,6],
		     [8,9,1,6,7]],
		    [[8,6,7,9,1],
		     [8,6,7,1,9],
		     [8,6,9,7,1],
		     [8,6,9,1,7],
		     [8,6,1,7,9],
		     [8,6,1,9,7]],
		    [[8,7,6,9,1],
		     [8,7,6,1,9],
		     [8,7,9,6,1],
		     [8,7,9,1,6],
		     [8,7,1,6,9],
		     [8,7,1,9,6]]]], 
		  perms:permute([1,9,6,7,8])).

perms_123456_test_() ->
    ?_assertEqual(
       [[
	 [[1,2,6,5,4,3],[1,2,6,5,3,4],[1,2,6,4,5,3],[1,2,6,4,3,5],[1,2,6,3,5,4],[1,2,6,3,4,5],[1,2,5,6,4,3],[1,2,5,6,3,4],[1,2,5,4,6,3],[1,2,5,4,3,6],[1,2,5,3,6,4],[1,2,5,3,4,6],[1,2,4,6,5,3],[1,2,4,6,3,5],[1,2,4,5,6,3],[1,2,4,5,3,6],[1,2,4,3,6,5],[1,2,4,3,5,6],[1,2,3,6,5,4],[1,2,3,6,4,5],[1,2,3,5,6,4],[1,2,3,5,4,6],[1,2,3,4,6,5],[1,2,3,4,5,6]],
	 
	 [[1,3,6,5,4,2],[1,3,6,5,2,4],[1,3,6,4,5,2],[1,3,6,4,2,5],[1,3,6,2,5,4],[1,3,6,2,4,5],[1,3,5,6,4,2],[1,3,5,6,2,4],[1,3,5,4,6,2],[1,3,5,4,2,6],[1,3,5,2,6,4],[1,3,5,2,4,6],[1,3,4,6,5,2],[1,3,4,6,2,5],[1,3,4,5,6,2],[1,3,4,5,2,6],[1,3,4,2,6,5],[1,3,4,2,5,6],[1,3,2,6,5,4],[1,3,2,6,4,5],[1,3,2,5,6,4],[1,3,2,5,4,6],[1,3,2,4,6,5],[1,3,2,4,5,6]]
	,[[1,4,6,5,3,2],[1,4,6,5,2,3],[1,4,6,3,5,2],[1,4,6,3,2,5],[1,4,6,2,5,3],[1,4,6,2,3,5],[1,4,5,6,3,2],[1,4,5,6,2,3],[1,4,5,3,6,2],[1,4,5,3,2,6],[1,4,5,2,6,3],[1,4,5,2,3,6],[1,4,3,6,5,2],[1,4,3,6,2,5],[1,4,3,5,6,2],[1,4,3,5,2,6],[1,4,3,2,6,5],[1,4,3,2,5,6],[1,4,2,6,5,3],[1,4,2,6,3,5],[1,4,2,5,6,3],[1,4,2,5,3,6],[1,4,2,3,6,5],[1,4,2,3,5,6]],
	 [[1,5,6,4,3,2],[1,5,6,4,2,3],[1,5,6,3,4,2],[1,5,6,3,2,4],[1,5,6,2,4,3],[1,5,6,2,3,4],[1,5,4,6,3,2],[1,5,4,6,2,3],[1,5,4,3,6,2],[1,5,4,3,2,6],[1,5,4,2,6,3],[1,5,4,2,3,6],[1,5,3,6,4,2],[1,5,3,6,2,4],[1,5,3,4,6,2],[1,5,3,4,2,6],[1,5,3,2,6,4],[1,5,3,2,4,6],[1,5,2,6,4,3],[1,5,2,6,3,4],[1,5,2,4,6,3],[1,5,2,4,3,6],[1,5,2,3,6,4],[1,5,2,3,4,6]],
	 [[1,6,5,4,3,2],[1,6,5,4,2,3],[1,6,5,3,4,2],[1,6,5,3,2,4],[1,6,5,2,4,3],[1,6,5,2,3,4],[1,6,4,5,3,2],[1,6,4,5,2,3],[1,6,4,3,5,2],[1,6,4,3,2,5],[1,6,4,2,5,3],[1,6,4,2,3,5],[1,6,3,5,4,2],[1,6,3,5,2,4],[1,6,3,4,5,2],[1,6,3,4,2,5],[1,6,3,2,5,4],[1,6,3,2,4,5],[1,6,2,5,4,3],[1,6,2,5,3,4],[1,6,2,4,5,3],[1,6,2,4,3,5],[1,6,2,3,5,4],[1,6,2,3,4,5]]],
	[[[2,1,6,5,4,3],[2,1,6,5,3,4],[2,1,6,4,5,3],[2,1,6,4,3,5],[2,1,6,3,5,4],[2,1,6,3,4,5],[2,1,5,6,4,3],[2,1,5,6,3,4],[2,1,5,4,6,3],[2,1,5,4,3,6],[2,1,5,3,6,4],[2,1,5,3,4,6],[2,1,4,6,5,3],[2,1,4,6,3,5],[2,1,4,5,6,3],[2,1,4,5,3,6],[2,1,4,3,6,5],[2,1,4,3,5,6],[2,1,3,6,5,4],[2,1,3,6,4,5],[2,1,3,5,6,4],[2,1,3,5,4,6],[2,1,3,4,6,5],[2,1,3,4,5,6]],
	 [[2,3,6,5,4,1],[2,3,6,5,1,4],[2,3,6,4,5,1],[2,3,6,4,1,5],[2,3,6,1,5,4],[2,3,6,1,4,5],[2,3,5,6,4,1],[2,3,5,6,1,4],[2,3,5,4,6,1],[2,3,5,4,1,6],[2,3,5,1,6,4],[2,3,5,1,4,6],[2,3,4,6,5,1],[2,3,4,6,1,5],[2,3,4,5,6,1],[2,3,4,5,1,6],[2,3,4,1,6,5],[2,3,4,1,5,6],[2,3,1,6,5,4],[2,3,1,6,4,5],[2,3,1,5,6,4],[2,3,1,5,4,6],[2,3,1,4,6,5],[2,3,1,4,5,6]],
	 [[2,4,6,5,3,1],[2,4,6,5,1,3],[2,4,6,3,5,1],[2,4,6,3,1,5],[2,4,6,1,5,3],[2,4,6,1,3,5],[2,4,5,6,3,1],[2,4,5,6,1,3],[2,4,5,3,6,1],[2,4,5,3,1,6],[2,4,5,1,6,3],[2,4,5,1,3,6],[2,4,3,6,5,1],[2,4,3,6,1,5],[2,4,3,5,6,1],[2,4,3,5,1,6],[2,4,3,1,6,5],[2,4,3,1,5,6],[2,4,1,6,5,3],[2,4,1,6,3,5],[2,4,1,5,6,3],[2,4,1,5,3,6],[2,4,1,3,6,5],[2,4,1,3,5,6]],
	 [[2,5,6,4,3,1],[2,5,6,4,1,3],[2,5,6,3,4,1],[2,5,6,3,1,4],[2,5,6,1,4,3],[2,5,6,1,3,4],[2,5,4,6,3,1],[2,5,4,6,1,3],[2,5,4,3,6,1],[2,5,4,3,1,6],[2,5,4,1,6,3],[2,5,4,1,3,6],[2,5,3,6,4,1],[2,5,3,6,1,4],[2,5,3,4,6,1],[2,5,3,4,1,6],[2,5,3,1,6,4],[2,5,3,1,4,6],[2,5,1,6,4,3],[2,5,1,6,3,4],[2,5,1,4,6,3],[2,5,1,4,3,6],[2,5,1,3,6,4],[2,5,1,3,4,6]],
	 [[2,6,5,4,3,1],[2,6,5,4,1,3],[2,6,5,3,4,1],[2,6,5,3,1,4],[2,6,5,1,4,3],[2,6,5,1,3,4],[2,6,4,5,3,1],[2,6,4,5,1,3],[2,6,4,3,5,1],[2,6,4,3,1,5],[2,6,4,1,5,3],[2,6,4,1,3,5],[2,6,3,5,4,1],[2,6,3,5,1,4],[2,6,3,4,5,1],[2,6,3,4,1,5],[2,6,3,1,5,4],[2,6,3,1,4,5],[2,6,1,5,4,3],[2,6,1,5,3,4],[2,6,1,4,5,3],[2,6,1,4,3,5],[2,6,1,3,5,4],[2,6,1,3,4,5]]],
	[[[3,1,6,5,4,2],[3,1,6,5,2,4],[3,1,6,4,5,2],[3,1,6,4,2,5],[3,1,6,2,5,4],[3,1,6,2,4,5],[3,1,5,6,4,2],[3,1,5,6,2,4],[3,1,5,4,6,2],[3,1,5,4,2,6],[3,1,5,2,6,4],[3,1,5,2,4,6],[3,1,4,6,5,2],[3,1,4,6,2,5],[3,1,4,5,6,2],[3,1,4,5,2,6],[3,1,4,2,6,5],[3,1,4,2,5,6],[3,1,2,6,5,4],[3,1,2,6,4,5],[3,1,2,5,6,4],[3,1,2,5,4,6],[3,1,2,4,6,5],[3,1,2,4,5,6]],
	 [[3,2,6,5,4,1],[3,2,6,5,1,4],[3,2,6,4,5,1],[3,2,6,4,1,5],[3,2,6,1,5,4],[3,2,6,1,4,5],[3,2,5,6,4,1],[3,2,5,6,1,4],[3,2,5,4,6,1],[3,2,5,4,1,6],[3,2,5,1,6,4],[3,2,5,1,4,6],[3,2,4,6,5,1],[3,2,4,6,1,5],[3,2,4,5,6,1],[3,2,4,5,1,6],[3,2,4,1,6,5],[3,2,4,1,5,6],[3,2,1,6,5,4],[3,2,1,6,4,5],[3,2,1,5,6,4],[3,2,1,5,4,6],[3,2,1,4,6,5],[3,2,1,4,5,6]],
	 [[3,4,6,5,2,1],[3,4,6,5,1,2],[3,4,6,2,5,1],[3,4,6,2,1,5],[3,4,6,1,5,2],[3,4,6,1,2,5],[3,4,5,6,2,1],[3,4,5,6,1,2],[3,4,5,2,6,1],[3,4,5,2,1,6],[3,4,5,1,6,2],[3,4,5,1,2,6],[3,4,2,6,5,1],[3,4,2,6,1,5],[3,4,2,5,6,1],[3,4,2,5,1,6],[3,4,2,1,6,5],[3,4,2,1,5,6],[3,4,1,6,5,2],[3,4,1,6,2,5],[3,4,1,5,6,2],[3,4,1,5,2,6],[3,4,1,2,6,5],[3,4,1,2,5,6]],
	 [[3,5,6,4,2,1],[3,5,6,4,1,2],[3,5,6,2,4,1],[3,5,6,2,1,4],[3,5,6,1,4,2],[3,5,6,1,2,4],[3,5,4,6,2,1],[3,5,4,6,1,2],[3,5,4,2,6,1],[3,5,4,2,1,6],[3,5,4,1,6,2],[3,5,4,1,2,6],[3,5,2,6,4,1],[3,5,2,6,1,4],[3,5,2,4,6,1],[3,5,2,4,1,6],[3,5,2,1,6,4],[3,5,2,1,4,6],[3,5,1,6,4,2],[3,5,1,6,2,4],[3,5,1,4,6,2],[3,5,1,4,2,6],[3,5,1,2,6,4],[3,5,1,2,4,6]],
	 [[3,6,5,4,2,1],[3,6,5,4,1,2],[3,6,5,2,4,1],[3,6,5,2,1,4],[3,6,5,1,4,2],[3,6,5,1,2,4],[3,6,4,5,2,1],[3,6,4,5,1,2],[3,6,4,2,5,1],[3,6,4,2,1,5],[3,6,4,1,5,2],[3,6,4,1,2,5],[3,6,2,5,4,1],[3,6,2,5,1,4],[3,6,2,4,5,1],[3,6,2,4,1,5],[3,6,2,1,5,4],[3,6,2,1,4,5],[3,6,1,5,4,2],[3,6,1,5,2,4],[3,6,1,4,5,2],[3,6,1,4,2,5],[3,6,1,2,5,4],[3,6,1,2,4,5]]],
	[[[4,1,6,5,3,2],[4,1,6,5,2,3],[4,1,6,3,5,2],[4,1,6,3,2,5],[4,1,6,2,5,3],[4,1,6,2,3,5],[4,1,5,6,3,2],[4,1,5,6,2,3],[4,1,5,3,6,2],[4,1,5,3,2,6],[4,1,5,2,6,3],[4,1,5,2,3,6],[4,1,3,6,5,2],[4,1,3,6,2,5],[4,1,3,5,6,2],[4,1,3,5,2,6],[4,1,3,2,6,5],[4,1,3,2,5,6],[4,1,2,6,5,3],[4,1,2,6,3,5],[4,1,2,5,6,3],[4,1,2,5,3,6],[4,1,2,3,6,5],[4,1,2,3,5,6]],
	 [[4,2,6,5,3,1],[4,2,6,5,1,3],[4,2,6,3,5,1],[4,2,6,3,1,5],[4,2,6,1,5,3],[4,2,6,1,3,5],[4,2,5,6,3,1],[4,2,5,6,1,3],[4,2,5,3,6,1],[4,2,5,3,1,6],[4,2,5,1,6,3],[4,2,5,1,3,6],[4,2,3,6,5,1],[4,2,3,6,1,5],[4,2,3,5,6,1],[4,2,3,5,1,6],[4,2,3,1,6,5],[4,2,3,1,5,6],[4,2,1,6,5,3],[4,2,1,6,3,5],[4,2,1,5,6,3],[4,2,1,5,3,6],[4,2,1,3,6,5],[4,2,1,3,5,6]],
	 [[4,3,6,5,2,1],[4,3,6,5,1,2],[4,3,6,2,5,1],[4,3,6,2,1,5],[4,3,6,1,5,2],[4,3,6,1,2,5],[4,3,5,6,2,1],[4,3,5,6,1,2],[4,3,5,2,6,1],[4,3,5,2,1,6],[4,3,5,1,6,2],[4,3,5,1,2,6],[4,3,2,6,5,1],[4,3,2,6,1,5],[4,3,2,5,6,1],[4,3,2,5,1,6],[4,3,2,1,6,5],[4,3,2,1,5,6],[4,3,1,6,5,2],[4,3,1,6,2,5],[4,3,1,5,6,2],[4,3,1,5,2,6],[4,3,1,2,6,5],[4,3,1,2,5,6]],
	 [[4,5,6,3,2,1],[4,5,6,3,1,2],[4,5,6,2,3,1],[4,5,6,2,1,3],[4,5,6,1,3,2],[4,5,6,1,2,3],[4,5,3,6,2,1],[4,5,3,6,1,2],[4,5,3,2,6,1],[4,5,3,2,1,6],[4,5,3,1,6,2],[4,5,3,1,2,6],[4,5,2,6,3,1],[4,5,2,6,1,3],[4,5,2,3,6,1],[4,5,2,3,1,6],[4,5,2,1,6,3],[4,5,2,1,3,6],[4,5,1,6,3,2],[4,5,1,6,2,3],[4,5,1,3,6,2],[4,5,1,3,2,6],[4,5,1,2,6,3],[4,5,1,2,3,6]],
	 [[4,6,5,3,2,1],[4,6,5,3,1,2],[4,6,5,2,3,1],[4,6,5,2,1,3],[4,6,5,1,3,2],[4,6,5,1,2,3],[4,6,3,5,2,1],[4,6,3,5,1,2],[4,6,3,2,5,1],[4,6,3,2,1,5],[4,6,3,1,5,2],[4,6,3,1,2,5],[4,6,2,5,3,1],[4,6,2,5,1,3],[4,6,2,3,5,1],[4,6,2,3,1,5],[4,6,2,1,5,3],[4,6,2,1,3,5],[4,6,1,5,3,2],[4,6,1,5,2,3],[4,6,1,3,5,2],[4,6,1,3,2,5],[4,6,1,2,5,3],[4,6,1,2,3,5]]],
	[[[5,1,6,4,3,2],[5,1,6,4,2,3],[5,1,6,3,4,2],[5,1,6,3,2,4],[5,1,6,2,4,3],[5,1,6,2,3,4],[5,1,4,6,3,2],[5,1,4,6,2,3],[5,1,4,3,6,2],[5,1,4,3,2,6],[5,1,4,2,6,3],[5,1,4,2,3,6],[5,1,3,6,4,2],[5,1,3,6,2,4],[5,1,3,4,6,2],[5,1,3,4,2,6],[5,1,3,2,6,4],[5,1,3,2,4,6],[5,1,2,6,4,3],[5,1,2,6,3,4],[5,1,2,4,6,3],[5,1,2,4,3,6],[5,1,2,3,6,4],[5,1,2,3,4,6]],[[5,2,6,4,3,1],[5,2,6,4,1,3],[5,2,6,3,4,1],[5,2,6,3,1,4],[5,2,6,1,4,3],[5,2,6,1,3,4],[5,2,4,6,3,1],[5,2,4,6,1,3],[5,2,4,3,6,1],[5,2,4,3,1,6],[5,2,4,1,6,3],[5,2,4,1,3,6],[5,2,3,6,4,1],[5,2,3,6,1,4],[5,2,3,4,6,1],[5,2,3,4,1,6],[5,2,3,1,6,4],[5,2,3,1,4,6],[5,2,1,6,4,3],[5,2,1,6,3,4],[5,2,1,4,6,3],[5,2,1,4,3,6],[5,2,1,3,6,4],[5,2,1,3,4,6]],[[5,3,6,4,2,1],[5,3,6,4,1,2],[5,3,6,2,4,1],[5,3,6,2,1,4],[5,3,6,1,4,2],[5,3,6,1,2,4],[5,3,4,6,2,1],[5,3,4,6,1,2],[5,3,4,2,6,1],[5,3,4,2,1,6],[5,3,4,1,6,2],[5,3,4,1,2,6],[5,3,2,6,4,1],[5,3,2,6,1,4],[5,3,2,4,6,1],[5,3,2,4,1,6],[5,3,2,1,6,4],[5,3,2,1,4,6],[5,3,1,6,4,2],[5,3,1,6,2,4],[5,3,1,4,6,2],[5,3,1,4,2,6],[5,3,1,2,6,4],[5,3,1,2,4,6]],[[5,4,6,3,2,1],[5,4,6,3,1,2],[5,4,6,2,3,1],[5,4,6,2,1,3],[5,4,6,1,3,2],[5,4,6,1,2,3],[5,4,3,6,2,1],[5,4,3,6,1,2],[5,4,3,2,6,1],[5,4,3,2,1,6],[5,4,3,1,6,2],[5,4,3,1,2,6],[5,4,2,6,3,1],[5,4,2,6,1,3],[5,4,2,3,6,1],[5,4,2,3,1,6],[5,4,2,1,6,3],[5,4,2,1,3,6],[5,4,1,6,3,2],[5,4,1,6,2,3],[5,4,1,3,6,2],[5,4,1,3,2,6],[5,4,1,2,6,3],[5,4,1,2,3,6]],[[5,6,4,3,2,1],[5,6,4,3,1,2],[5,6,4,2,3,1],[5,6,4,2,1,3],[5,6,4,1,3,2],[5,6,4,1,2,3],[5,6,3,4,2,1],[5,6,3,4,1,2],[5,6,3,2,4,1],[5,6,3,2,1,4],[5,6,3,1,4,2],[5,6,3,1,2,4],[5,6,2,4,3,1],[5,6,2,4,1,3],[5,6,2,3,4,1],[5,6,2,3,1,4],[5,6,2,1,4,3],[5,6,2,1,3,4],[5,6,1,4,3,2],[5,6,1,4,2,3],[5,6,1,3,4,2],[5,6,1,3,2,4],[5,6,1,2,4,3],[5,6,1,2,3,4]]],[[[6,1,5,4,3,2],[6,1,5,4,2,3],[6,1,5,3,4,2],[6,1,5,3,2,4],[6,1,5,2,4,3],[6,1,5,2,3,4],[6,1,4,5,3,2],[6,1,4,5,2,3],[6,1,4,3,5,2],[6,1,4,3,2,5],[6,1,4,2,5,3],[6,1,4,2,3,5],[6,1,3,5,4,2],[6,1,3,5,2,4],[6,1,3,4,5,2],[6,1,3,4,2,5],[6,1,3,2,5,4],[6,1,3,2,4,5],[6,1,2,5,4,3],[6,1,2,5,3,4],[6,1,2,4,5,3],[6,1,2,4,3,5],[6,1,2,3,5,4],[6,1,2,3,4,5]],[[6,2,5,4,3,1],[6,2,5,4,1,3],[6,2,5,3,4,1],[6,2,5,3,1,4],[6,2,5,1,4,3],[6,2,5,1,3,4],[6,2,4,5,3,1],[6,2,4,5,1,3],[6,2,4,3,5,1],[6,2,4,3,1,5],[6,2,4,1,5,3],[6,2,4,1,3,5],[6,2,3,5,4,1],[6,2,3,5,1,4],[6,2,3,4,5,1],[6,2,3,4,1,5],[6,2,3,1,5,4],[6,2,3,1,4,5],[6,2,1,5,4,3],[6,2,1,5,3,4],[6,2,1,4,5,3],[6,2,1,4,3,5],[6,2,1,3,5,4],[6,2,1,3,4,5]],[[6,3,5,4,2,1],[6,3,5,4,1,2],[6,3,5,2,4,1],[6,3,5,2,1,4],[6,3,5,1,4,2],[6,3,5,1,2,4],[6,3,4,5,2,1],[6,3,4,5,1,2],[6,3,4,2,5,1],[6,3,4,2,1,5],[6,3,4,1,5,2],[6,3,4,1,2,5],[6,3,2,5,4,1],[6,3,2,5,1,4],[6,3,2,4,5,1],[6,3,2,4,1,5],[6,3,2,1,5,4],[6,3,2,1,4,5],[6,3,1,5,4,2],[6,3,1,5,2,4],[6,3,1,4,5,2],[6,3,1,4,2,5],[6,3,1,2,5,4],[6,3,1,2,4,5]],[[6,4,5,3,2,1],[6,4,5,3,1,2],[6,4,5,2,3,1],[6,4,5,2,1,3],[6,4,5,1,3,2],[6,4,5,1,2,3],[6,4,3,5,2,1],[6,4,3,5,1,2],[6,4,3,2,5,1],[6,4,3,2,1,5],[6,4,3,1,5,2],[6,4,3,1,2,5],[6,4,2,5,3,1],[6,4,2,5,1,3],[6,4,2,3,5,1],[6,4,2,3,1,5],[6,4,2,1,5,3],[6,4,2,1,3,5],[6,4,1,5,3,2],[6,4,1,5,2,3],[6,4,1,3,5,2],[6,4,1,3,2,5],[6,4,1,2,5,3],[6,4,1,2,3,5]],[[6,5,4,3,2,1],[6,5,4,3,1,2],[6,5,4,2,3,1],[6,5,4,2,1,3],[6,5,4,1,3,2],[6,5,4,1,2,3],[6,5,3,4,2,1],[6,5,3,4,1,2],[6,5,3,2,4,1],[6,5,3,2,1,4],[6,5,3,1,4,2],[6,5,3,1,2,4],[6,5,2,4,3,1],[6,5,2,4,1,3],[6,5,2,3,4,1],[6,5,2,3,1,4],[6,5,2,1,4,3],[6,5,2,1,3,4],[6,5,1,4,3,2],[6,5,1,4,2,3],[6,5,1,3,4,2],[6,5,1,3,2,4],[6,5,1,2,4,3],[6,5,1,2,3,4]]]]
		 , perms:permute([1,2,3,4,5,6])).

