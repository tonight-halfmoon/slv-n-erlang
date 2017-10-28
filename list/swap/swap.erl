-module(swap).
-export([swap/1, swap_2500_profile/0]).
-include_lib("eunit/include/eunit.hrl").

%%% auto type possible ascii io:fwrite("", [swap:swap_tail2(lists:seq(1,2500))]).

swap([]) ->
    [];
swap([X]) ->
    [X];
swap([X,Y]) ->
    [[Y,X],[X,Y]];
swap([X,Y,Z]) ->
    [[Y, X, Z], [Z, Y, X], [X, Z, Y], [Z,X,Y], [X,Y,Z], [Y,Z,X]];
swap(L) ->
    {Left, Right} = split(L),
    lists:map(fun(Z) -> lists:flatten(Z) end, [[X|Y] || X <- swap(Left), Y <- swap(Right)]).
    %[concat(X, Y) || X <- swap(Left), Y <- swap(Right)].
   
    %%% (!) timeout with using function concat/2 -> optimise!
    %swap([], [dist_tail:dist_tail(swap(Left), swap(Right))|S]).

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
     {X, _O} = timer:tc(?MODULE, swap, [lists:seq(1, 2500)]), 
     io:fwrite("Profile: ~p microsends~n", [X]).

swap_20_test_() ->
    {"Swap '[1..20]' must halt", 
     ?_assertMatch([L|_] when length(L) == 20, swap(lists:seq(1, 20)))}.

swap_20_tot_test_() ->
    {"Swap '[1..20]' must halt", 
     ?_assertMatch(L when length(L) == 20736, swap(lists:seq(1, 20)))}.

swap_35_tot_test_() ->
    {"Swap '[1..35]' must halt", 
     ?_assertMatch(L when length(L) == 1769472, swap(lists:seq(1, 35)))}.

%swap_2500_test_() ->
%    {"Swap '[1..2500]' must halt", 
    %?_assertMatch([[L|_]|_] when length(L) == 14520, swap(lists:seq(1, 2500)))}.
    %?_assertMatch([[[L|_]|_]] when length(L) == 14520, swap(lists:seq(1, 2500)))}.

swap_test_one_test_() ->
    {"Swap [a] must yeild in [a]", ?_assertEqual([a], swap([a]))}.

swap_test_() ->
    {"swapping '[1,2,3]' must result in '[2,1,3], [3,2,1], [1,3,2]'", 
     ?_assertEqual([[2,1,3], [3,2,1], [1,3,2], [3,1,2],[1,2,3],[2,3,1]], swap([1,2,3]))}.

swap_empty_test_() ->
    {"swapping '[]' must result in '[]'", 
     ?_assertEqual([], swap([]))}.

swap_7_elem_list_test_() ->
    {"swap [1,2,3,4,5,6,7] must yield in a list of size 12 ",
     ?_assertMatch( L when 24 == length(L), swap([1,2,3,4,5,6,7]))}.

swap_7_elem_list_expected_test_() ->
    {"swap [1,2,3,4,5,6,7] must halt and equal expected",
     ?_assertEqual(
	[[2,3,1,4,5,7,6],
	 [2,3,1,4,5,6,7],
	 [2,3,1,5,4,7,6],
	 [2,3,1,5,4,6,7],
	 [1,2,3,4,5,7,6],
	 [1,2,3,4,5,6,7],
	 [1,2,3,5,4,7,6],
	 [1,2,3,5,4,6,7],
	 [3,1,2,4,5,7,6],
	 [3,1,2,4,5,6,7],
	 [3,1,2,5,4,7,6],
	 [3,1,2,5,4,6,7],
	 [1,3,2,4,5,7,6],
	 [1,3,2,4,5,6,7],
	 [1,3,2,5,4,7,6],
	 [1,3,2,5,4,6,7],
	 [3,2,1,4,5,7,6],
	 [3,2,1,4,5,6,7],
	 [3,2,1,5,4,7,6],
	 [3,2,1,5,4,6,7],
	 [2,1,3,4,5,7,6],
	 [2,1,3,4,5,6,7],
	 [2,1,3,5,4,7,6],
	 [2,1,3,5,4,6,7]],
	swap([1,2,3,4,5,6,7]))}.

%%% (!) Why with 8-elem list produces less than for 7-elem list?
swap_8_elem_list_test_() ->
    {"swap [1,2,3,4,5,6,7,8] must halt and match certain expectation",
     ?_assertMatch([H|_] when H == [4,3,1,2,5,6,8,7], swap([1,2,3,4,5,6,7,8]))}.

swap_8_elem_list_return_list_size_16_test_() ->
    {"Swap 8-elem list must return a list of lists of size '16'", 
     ?_assertEqual(16, length(swap([1,2,3,4,5,6,7,8])))}.

swap_8_elem_list_return_as_expected_test_() ->
    {"Swap 8-elem list must return expected lists", 
     ?_assertEqual(
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
	swap([1,2,3,4,5,6,7,8]))}.
