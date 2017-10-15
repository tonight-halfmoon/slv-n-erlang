%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2017, 
%%% @doc
%%%
%%% @end
%%% Created : 11 Oct 2017 by  <rosemary@SCUBA>

-module(swap_permutation).
-author('rosemary@SCUBA').
-export([main/0, swp_perms/2]).
-include_lib("eunit/include/eunit.hrl").
-import(n_swaps, [nswp/2]).
-import(n_adjacent_swaps, [nas/2]).
-import(dist, [dist/2]).
-import(mod, [mod/2]).
-define(MOD, 1000000007).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

swp_perms(L, K) ->
    S1 = length(n_adjacent_swaps:nas(L, K)),
    S2 = length(n_swaps:nswp(L, K)),
    {mod(S1, ?MOD), mod(S2, ?MOD)}.

main() ->
    {ok, [N, K]} = io:fread("", "~d~d"),
    [io:fwrite("~p ", [Fet]) || Fet <- tuple_to_list(swp_perms(lists:seq(1, N), K))],
    true.

swp_perms_test_() ->
    {"swp perms of '[1,2,3] and 2' must yield in '{3, 6}'", ?_assertEqual({3,6}, swp_perms([1,2,3], 2))}.

swp_perms_2500_2500_test_() ->
    {"swp perms of '[1..19] and 4' must yield in '{22, 88}'", ?_assertEqual({5,12}, swp_perms(lists:seq(1, 19), 4))}.

swap_2500_test_() ->
    {"N Swap Permutations '[1..2500], 2500' must halt", 
    ?_assertEqual({2501, 7500}, swp_perms(lists:seq(1, 2500), 2500))}.
