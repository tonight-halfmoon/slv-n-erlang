-module(conc_t).
-export([conc_t/2]).
-include_lib("eunit/include/eunit.hrl").

%%% (!) Does it scale up. Yes, up to 2500x2500.

conc_t_2500_test_() ->
    {"2500 and 2500 must halt", ?_assertMatch(L when length(L) == 5000, conc_t(lists:seq(1,2500), lists:seq(1,2500))) }.

conc_t_2500x2500_test_() ->
    {"2500x2500 and 2500x1500 must halt", ?_assertMatch(L when length(L) == 5000, conc_t([lists:seq(1, 2500) || _ <- lists:seq(1, 2500)], [lists:seq(1, 2500) || _ <- lists:seq(1, 2500)])) }.

conc_t_test_() ->
    {"", ?_assertEqual([1,2,4,5], conc_t([1,2],[4,5]))}.


conc_t__test_() ->
    {"", ?_assertEqual([[1,2],[9,8,0],[4,5]], conc_t([[1,2], [9,8,0]], [[4,5]]))}.


conc_t___test_() ->
    {"", ?_assertEqual(
	    [[a,b,c,d],
	     [x,y,z],
	     [i,u,y],
	     [9,8,0,7],
	     [1,2,3],
	     [15,16,99],
	     [p,s,q]
	    ], 
	    conc_t([[1,2,3],[9,8,0,7],[15,16,99],[p,s,q]], 
		   [[i,u,y],[x,y,z],[a,b,c,d]]
		  ))}.

conc_t(X, Y) when length(X) == 1 orelse length(Y) == 1 ->
    lists:append(X, Y);
conc_t(X, Y) when length(X) == 2 orelse length(Y) == 2 ->
    lists:append(X, Y);
conc_t(L1, L2) ->
    conc_t(L1, L2, []).

conc_t([], L2, L) ->
    lists:append(L2, L);
conc_t(L1, [], L) ->
    lists:append(L1, L);
conc_t(L1, L2, C) ->
    {Left1, Right1} = split(L1),
    {Left2, Right2} = split(L2),
    conc_t([], [], concat(concat(conc_t(Left1, Right1), conc_t(Left2, Right2)),C)).



concat([], L) ->
     L;
concat([HoL|ToL], L) ->
     concat(ToL, [HoL|L]).

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
