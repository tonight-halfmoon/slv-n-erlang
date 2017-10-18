-module(swap_count).
-export([swap_tail2/1, swap_2500_profile/0]).
-include_lib("eunit/include/eunit.hrl").

swap_tail2_i([], S) ->
    S;
swap_tail2_i([_], S) ->
     1 + S;
swap_tail2_i([_,_], S) ->
    2 + S;
swap_tail2_i([_,_,_], S) ->
    3 + S;
swap_tail2_i(L, S) ->
    swap_tail2(L, S).

swap_tail2(L) ->
    swap_tail2(L, 1).

swap_tail2([], S) ->
    S;
swap_tail2([_], S)->
    S;
swap_tail2([_, _], S) ->
    2 * S;
swap_tail2([_, _, _], S) ->
    3 * S;
swap_tail2(L, S) ->
    {Left, Right} = split(L),
    %io:fwrite("Left: ~w~n", [Left]),
    %io:fwrite("Right: ~w~n", [Right]),
    swap_tail2_i(Left, S) * swap_tail2_i(Right, S).
    
split([]) ->
    [];
split([_]) ->
    [];
split([H|T]) -> 
    split(T, [H]).

split(L, R) when length(L) =:= length(R);
		 length(L) =:= length(R) + 1 ->
    {R, L};
split([H|T], R) ->
    split(T, [H|R]).

swap_2500_profile() ->
     {X, _O} = timer:tc(?MODULE, swap_tail2, [lists:seq(1, 2500)]), 
     io:fwrite("Profile: ~p microsends~n", [X]).

swap_test_one_test_() ->
    {"Swap [a] must yeild in [a]", ?_assertEqual([a], swap_tail2([a]))}.

swap_tail2_test_() ->
    {"swapping '[1,2,3]' must result in '[2,1,3], [3,2,1], [1,3,2]'", 
     ?_assertEqual([[2,1,3], [3,2,1], [1,3,2]], swap_tail2([1,2,3]))}.

swap2_test_() ->
    {"swapping '[1,2,3]' must result in '[2,1,3], [3,2,1], [1,3,2]'", 
     ?_assertEqual([[2,1,3], [3,2,1], [1,3,2]], swap_tail2([1,2,3]))}.


swap_empty_test_() ->
    {"swapping '[]' must result in '[]'", 
     ?_assertEqual([], swap_tail2([]))}.

swap_7_elem_list_test_() ->
    {"swap [1,2,3,4,5,6,7] must halt and equal expected",
     ?_assertEqual(
	[],lists:subtract([[7,3,1,2,6,5,4],
	 [7,3,1,2,5,4,6],
	 [7,3,1,2,4,6,5],
	 [7,1,2,3,6,5,4],
	 [7,1,2,3,5,4,6],
	 [7,1,2,3,4,6,5],
	 [7,2,3,1,6,5,4],
	 [7,2,3,1,5,4,6],
	 [7,2,3,1,4,6,5]],
	swap_tail2([1,2,3,4,5,6,7])))}.

swap_tail2_7_elem_list_test_() ->
    {"swap [1,2,3,4,5,6,7] must halt and equal expected",
     ?_assertEqual(
	[[1,3,2,5,4,6,7],
	 [1,3,2,5,4,7,6],
	 [2,1,3,5,4,6,7],
	 [2,1,3,5,4,7,6],
	 [3,2,1,5,4,7,6],
	 [3,2,1,5,4,6,7],
	 [3,2,1,4,5,6,7],
	 [3,2,1,4,5,7,6],
	 [2,1,3,4,5,7,6],
	 [2,1,3,4,5,6,7],
	 [1,3,2,4,5,7,6],
	 [1,3,2,4,5,6,7]],
	swap_tail2([1,2,3,4,5,6,7]))}.

swap2_8_elem_list_test_() ->
    {"swap [1,2,3,4,5,6,7,8] must halt and match certain expectation",
     ?_assertMatch([H|_] when H == [5,6,4,2,1,3,8,7], swap_tail2([1,2,3,4,5,6,7,8]))}.

swap_tail2_8_elem_list_test_() ->
    {"swap [1,2,3,4,5,6,7,8] must halt and match certain expectation",
     ?_assertMatch([H|_] when H == [7,8,5,6,3,4,2,1], swap_tail2([1,2,3,4,5,6,7,8]))}.


swap_2500_test_() ->
    {"Swap '[1..2500]' must halt", 
    %?_assertMatch([[L|_]|_] when length(L) == 14520, swap_tail2(lists:seq(1, 2500)))}.
     ?_assertMatch([[[L|_]|_]] when length(L) == 14520, swap_tail2(lists:seq(1, 2500)))}.
