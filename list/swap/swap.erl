-module(swap).
-export([swap_tail2/1, swap2/1, swap/1, swap_2500_profile/0]).
-include_lib("eunit/include/eunit.hrl").
-import(dist_tail, [dist_tail/2]).
-import(dist, [dist/2]).
-import(umerge, [umerge/2]).

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
    %{Left, Right} = lists:split(trunc(length(L)/2), L),
    swap_tail2([], concat(dist_tail:dist_tail(swap_tail2(Left), swap_tail2(Right)), S)).

swap([]) ->
    [];
swap([X]) ->
    [X];
swap([X,Y]) ->
    [[Y,X],[X,Y]];
swap([X,Y,Z]) ->
     %[[Y, X, Z], [Y,Z,X], [Z,X,Y], [Z, Y, X], [X, Z, Y], [X, Y, Z]];
    [[Y, X, Z], [Z, Y, X], [X, Z, Y]];
swap(L) ->
    {Left, Right} = split(L),
    dist:dist(swap(Left), swap(Right)).
    
swap2(L) ->
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
    swap(T, dist:dist(P, [[Y,X,Z],[Z,Y,X],[X,Z,Y]]));
swap([A], P) ->
    dist([A], P).

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
    {"Swap [a] must yeild in [a]", ?_assertEqual([a], swap([a]))}.

swap_tail2_test_() ->
    {"swapping '[1,2,3]' must result in '[2,1,3], [3,2,1], [1,3,2]'", 
     ?_assertEqual([[2,1,3], [3,2,1], [1,3,2]], swap_tail2([1,2,3]))}.

swap2_test_() ->
    {"swapping '[1,2,3]' must result in '[2,1,3], [3,2,1], [1,3,2]'", 
     ?_assertEqual([[2,1,3], [3,2,1], [1,3,2]], swap2([1,2,3]))}.


swap_empty_test_() ->
    {"swapping '[]' must result in '[]'", 
     ?_assertEqual([], swap([]))}.

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
	swap2([1,2,3,4,5,6,7])))}.

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
     ?_assertMatch([H|_] when H == [5,6,4,2,1,3,8,7], swap2([1,2,3,4,5,6,7,8]))}.

swap_tail2_8_elem_list_test_() ->
    {"swap [1,2,3,4,5,6,7,8] must halt and match certain expectation",
     ?_assertMatch([H|_] when H == [7,8,5,6,3,4,2,1], swap_tail2([1,2,3,4,5,6,7,8]))}.


%swap_2500_test_() ->
%    {"Swap '[1..2500]' must halt", 
%    ?_assertMatch([[L|_]|_] when length(L) == 2500, swap_tail2(lists:seq(1, 2500)))}.


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

concat([], L) ->
     L;
concat([HoL|ToL], L) ->
     concat(ToL, [HoL|L]).
