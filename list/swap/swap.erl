-module(swap).
-export([swap_tail2/1, swap_2500_profile/0]).
-include_lib("eunit/include/eunit.hrl").

%%% auto type possible ascii io:fwrite("", [swap:swap_tail2(lists:seq(1,2500))]).

swap_tail2([]) ->
    [];
swap_tail2([X]) ->
    [X];
swap_tail2([X,Y]) ->
    [[Y,X],[X,Y]];
swap_tail2([X,Y,Z]) ->
    [[Y, X, Z], [Z, Y, X], [X, Z, Y]];
swap_tail2(L) ->
    swap_tail2(L, []).

swap_tail2([], S) ->
    S;
swap_tail2(L, S) ->
    {Left, Right} = split(L),
   % swap_tail2([], concat(lists:map(fun(X) -> lists:map(fun(Y) -> lists:flatten(X,Y) end, swap_tail2(Left)) end, swap_tail2(Right)), S)).
    swap_tail2([], concat(lists:map(fun(Z) -> lists:flatten(Z) end, [[X|Y] || X <- swap_tail2(Left), Y <- swap_tail2(Right)]), S)).
    %%% (!) timeout with using function concat/2 -> optimise!
    %swap_tail2([], [dist_tail:dist_tail(swap_tail2(Left), swap_tail2(Right))|S]).

concat([], L) ->
     L;
concat([HoL|ToL], L) ->
     concat(ToL, [HoL|L]).

split([]) ->
    {[],[]};
split([S]) ->
    {[S],[]};
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

swap_empty_test_() ->
    {"swapping '[]' must result in '[]'", 
     ?_assertEqual([], swap_tail2([]))}.

swap_7_elem_list_test_() ->
    {"swap [1,2,3,4,5,6,7] must yield in a list of size 12 ",
     ?_assertMatch( L when 12 == length(L), swap_tail2([1,2,3,4,5,6,7]))}.

swap_tail2_7_elem_list_test_() ->
    {"swap [1,2,3,4,5,6,7] must halt and equal expected",
     ?_assertEqual(
	[[3,1,2,4,5,7,6],
	 [3,1,2,4,5,6,7],
	 [3,1,2,5,4,7,6],
	 [3,1,2,5,4,6,7],
	 [1,2,3,4,5,7,6],
	 [1,2,3,4,5,6,7],
	 [1,2,3,5,4,7,6],
	 [1,2,3,5,4,6,7],
	 [2,3,1,4,5,7,6],
	 [2,3,1,4,5,6,7],
	 [2,3,1,5,4,7,6],
	 [2,3,1,5,4,6,7]],
	swap_tail2([1,2,3,4,5,6,7]))}.

swap_tail2_8_elem_list_test_() ->
    {"swap [1,2,3,4,5,6,7,8] must halt and match certain expectation",
     ?_assertMatch([H|_] when H == [4,3,1,2,5,6,8,7], swap_tail2([1,2,3,4,5,6,7,8]))}.

swap_tail2_8_elem_list_return_list_size_16_test_() ->
    {"Swap 8-elem list must return a list of lists of size '16'", ?_assertEqual(16, length(swap_tail2([1,2,3,4,5,6,7,8])))}.

swap_tail2_8_elem_list_return_as_expected_test_() ->
    {"Swap 8-elem list must return expected lists", ?_assertEqual(
	    [[4,3,1,2,5,6,8,7],
	     [4,3,1,2,5,6,7,8],
	     [4,3,1,2,6,5,8,7],
	     [4,3,1,2,6,5,7,8],
	     [4,3,2,1,5,6,8,7],
	     [4,3,2,1,5,6,7,8],
	     [4,3,2,1,6,5,8,7],
	     [4,3,2,1,6,5,7,8],
	     [3,4,1,2,5,6,8,7],
	     [3,4,1,2,5,6,7,8],
	     [3,4,1,2,6,5,8,7],
	     [3,4,1,2,6,5,7,8],
	     [3,4,2,1,5,6,8,7],
	     [3,4,2,1,5,6,7,8],
	     [3,4,2,1,6,5,8,7],
	     [3,4,2,1,6,5,7,8]], 
	swap_tail2([1,2,3,4,5,6,7,8]))}.

%swap_2500_test_() ->
%    {"Swap '[1..2500]' must halt", 
     %?_assertMatch([[L|_]|_] when length(L) == 14520, swap_tail2(lists:seq(1, 2500)))}.
%    ?_assertMatch([[[L|_]|_]] when length(L) == 14520, swap_tail2(lists:seq(1, 2500)))}.
