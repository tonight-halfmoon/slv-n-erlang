-module(swap).
-export([swap/1, swap_2500_profile/0]).
-include_lib("eunit/include/eunit.hrl").
-import(dist, [dist/2]).
-import(umerge, [umerge/2]).

swap(L) ->
    swap(L, []).

swap([], P) ->
    P;
swap([X,Y], []) ->
    [[Y,X],[X,Y]];
swap([X,Y], P) ->
    dist:dist(P, [[Y,X], [X,Y]]);
swap([X,Y,Z|T], []) ->
    swap(T, [[Y, X, Z], [Z, Y, X], [X, Z, Y]]);
swap([X,Y,Z|T], P) ->
    swap(T, dist:dist(P, [[X,Z,Y],[Y,X,Z],[Z,Y,X]]));
swap([A], P) ->
    dist([A], P).

swap_test_() ->
    {"swapping '[1,2,3]' must result in '[2,1,3], [3,2,1], [1,3,2]'", 
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

swap_7_elem_list_test_() ->
    {"swap [1,2,3,4,5,6,7] must halt and equal expected", ?_assertEqual(
	[[5,6,4,7,2,1,3],
	 [5,6,4,7,3,2,1],
	 [5,6,4,7,1,3,2],
	 [4,5,6,7,2,1,3],
	 [4,5,6,7,3,2,1],
	 [4,5,6,7,1,3,2],
	 [6,4,5,7,2,1,3],
	 [6,4,5,7,3,2,1],
	 [6,4,5,7,1,3,2]],
	swap:swap([1,2,3,4,5,6,7]))}.

swap_8_elem_list_test_() ->
{"swap [1,2,3,4,5,6,7,8] must halt and match certain expectation",
 ?_assertMatch([H|_] when H == [4,5,6,2,1,3,8,7], swap([1,2,3,4,5,6,7,8]))}.


%% swap([]) ->
%%     [];
%% swap([X, Y, Z]) ->
%%     [[Y, X, Z], [Z, Y, X], [X, Z, Y]];
%% swap([X,Y]) ->
%%     [[Y,X], [X,Y]];
%% swap([X,Y,Z|[T]]) ->
%%     dist(T, swap([X,Y,Z]));
%% %swap([X, Y, Z|T]) ->
%% %    [[dist(dist(H, P), swap(T)) || P <- swap(T2)] || [H|T2] <- swap([X, Y, Z])];
%% swap([X, Y, Z|T]) ->
%%     [concat(St, S) || St <- swap(T), S <- swap([X,Y,Z])];
%% swap([A]) ->
%%     [A].

%% concat([], L) ->
%%     L;
%% concat([HoL|ToL], L) ->
%%     concat(ToL, [HoL|L]).
