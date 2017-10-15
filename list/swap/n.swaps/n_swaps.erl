-module(n_swaps).
-export([nswp/2, swap_2500_profile/0]).
-import(dist, [dist/2]).
-include_lib("eunit/include/eunit.hrl").
    
nswp(L, 1) ->
    swap(L);
nswp(L, K) when K > 1 andalso K =< length(L) ->
    Si = swap(L),
    nswpsi(Si, K, 1, [Si]);
nswp(_L, _K) ->
    ok.

nswpsi([], _K, _I, S) ->
    S;
nswpsi(_, K, K, S) ->
    S;
nswpsi(Si, K, I, S) ->
    [Ai|Ti] = Si,
    nswpsi(Ti, K, I + 1, [swap(Ai)|S]).

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

swap1([]) ->
    [];
swap1([X, Y, Z]) ->
    [Y, X, Z];
swap1([X, Y]) ->
    [Y, X];
swap1([X, Y, Z|T]) ->
    dist([Y, X, Z], swap1(T));
swap1([A]) ->
    [A].

concat([], L) ->
    L;
concat([HoL|ToL], L) ->
    concat(ToL, [HoL|L]).
 
swap_test_() ->
    {"Swapping '[1,2,3], 2' must result in '[1,2,3], [2,3,1] , [3,2,1]'", 
     ?_assertEqual([[[1,2,3], [3,1,2], [2,3,1]], [[2,1,3], [3,2,1], [1,3,2]]], nswp([1,2,3], 2))}.

swap_2500_test_() ->
    {"Swapping '[1..2500], 2500' must halt", 
    ?_assertMatch([[L|_]|_] when length(L) == 2500, nswp(lists:seq(1, 2500), 1))}.

swap_2500_profile() ->
     {X, _O} = timer:tc(?MODULE, nswp, [lists:seq(1, 2500), 2500]), 
     io:fwrite("Profile: ~p microsends~n", [X]).

%4> n_swaps:swap_2500_profile().
%Profile: 3 microsends
