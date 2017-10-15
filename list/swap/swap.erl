-module(swap).
-export([swap/1, swap_2500_profile/0]).
-include_lib("eunit/include/eunit.hrl").
-import(dist, [dist/2]).

swap_test_() ->
    {"swapping '[1,2,3]' must result in '[1,2,3], [2,1,3] , [3,1,2]'", 
     ?_assertEqual([[2,1,3], [3,2,1], [1,3,2]], swap([1,2,3]))}.

swap_empty_test_() ->
    {"swapping '[]' must result in '[]'", 
     ?_assertEqual([], swap([]))}.

swap_2500_test_() ->
    {"Swap '[1..2500]' must halt", 
    ?_assertMatch([[L|_]|_] when length(L) == 2500, swap(lists:seq(1, 2500)))}.

swap_2500_profile() ->
     {X, _O} = timer:tc(?MODULE, swap, [lists:seq(1, 2500)]), 
     io:fwrite("Profile: ~p microsends~n", [X]).

swap([]) ->
    [];
swap([X, Y, Z]) ->
    [[Y, X, Z], [Z, Y, X], [X, Z, Y]];
swap([X, Y]) ->
    [[Y, X], [X, Y]];
swap([X, Y, Z|T]) ->
    [[concat(dist(H, P), T) || P <- swap(T2)] || [H|T2] <- swap([X, Y, Z])];
swap([A]) ->
    [A].

concat([], L) ->
    L;
concat([HoL|ToL], L) ->
    concat(ToL, [HoL|L]).
