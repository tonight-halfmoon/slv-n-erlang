%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2017, 
%%% @doc
%%%
%%% @end
%%% Implemented: 11 Oct 2017 by  <rosemary@SCUBA>

-module(solution).
-author('rosemary@SCUBA').
-export([main/0, swp_perms/2]).
-include_lib("eunit/include/eunit.hrl").
-define(MOD, 1000000007).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

swp_perms_test_() ->
    {"swp perms of '[1,2,3] and 2' must yield in '{3, 6}'", ?_assertEqual({3,6}, swp_perms([1,2,3], 2))}.

swap_2500_test_() ->
    {"N Swap Permutations '[1..2500], 2500' must halt", 
    ?_assertEqual({2501, 7500}, swp_perms(lists:seq(1, 2500), 2500))}.

swp_perms(L, K) ->
    S1 = length(nas(L, K)),
    S2 = length(swp(L, K)),
    {mod(S1, ?MOD), mod(S2, ?MOD)}.

nas(L, 0) ->
    L;
nas(L, K) ->
    nas(L, K, 0, [L]).

nas(_L, K, K, S) ->
    S;
nas(L, K, I, S) ->
    Swpith = swap_adjacent(L), 
    nas(Swpith, K - 1, I, [Swpith|S]).


swap_adjacent([]) ->
    [];
swap_adjacent([A]) ->
    [A];
swap_adjacent([H, Next]) ->
    [Next, H];
swap_adjacent([H, Hn|[T]]) ->
    [Hn, T|[H]];
swap_adjacent([H, Hn|[Th, Tn]]) ->
    [Hn, Th| swap_adjacent([H, Tn])];
swap_adjacent([H, Hn|T]) ->
    [Hn, H| swap_adjacent(T)].

swp(L, K) ->
    nswp(L, K, 0, []). 

nswp([], _, _, S) ->
    S;
%nswp(L, 0, _, []) ->
%    L;
nswp(_L, K, K, S) ->
    S;
%% This logic consumes a lot of memory yields in a runtime error
nswp([L|T], K, I, S) when is_list(L) ->
    nswp(T, K, I, concat(S, nswp(L, K, I, S)));
nswp(L, K, I, _S) ->
    Swpd = swp(L),
    nswp(Swpd, K, I + 1, Swpd).

concat([], L) ->
    L;
concat([HoL|ToL], L) ->
    concat(ToL, [HoL|L]).

swp(L) ->
    swp(L, length(L), 1, []).

swp([], _, _, Slts) ->
    Slts;
swp(_L, M, M, Slts) ->
    Slts;
swp(L, M, J, Slts) ->
    ElemJ = lists:nth(J, L), 
    ElemM = lists:nth(M, L),
    SwpdJ = concat(ElemM, lists:delete(ElemM, lists:delete(ElemJ, L)), ElemJ),
    swp(L, M, J + 1, [SwpdJ|Slts]).

concat(L1, L2, L3) when is_list(L2) ->
    [L1|[L3|L2]].

main() ->
    {ok, [N, K]} = io:fread("", "~d~d"),
    [io:fwrite("~p ", [Fet]) || Fet <- tuple_to_list(swp_perms(lists:seq(1, N), K))],
    true.


mod(0,0) ->
    undefined; %% The divisor must not be 0.
mod(_,0) ->
    undefined; %% The divisor must not be 0.
mod(0,_) ->
    0;
mod(X,X) ->
    0; 
% mod(1, 1) -> 0;
% mod(1, _) -> 1;
mod(_, 1) ->
    0;
mod(X, Y) ->
    ((X rem Y) + Y) rem Y.
