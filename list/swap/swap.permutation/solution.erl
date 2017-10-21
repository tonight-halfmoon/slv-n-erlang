%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2017, 
%%% @doc
%%%
%%% @end
%%% Implemented: 11 Oct 2017 by  <rosemary@SCUBA>

-module(solution).
-author('rosemary@SCUBA').
-export([main/0, swp_perms/2, ski/2]).
-include_lib("eunit/include/eunit.hrl").
-define(MOD, 1000000007).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

swp_perms_test_() ->
    { 
      "swp perms of '[1,2,3] and 2' must yield in '{3, 6}'", 
      ?_assertEqual({3,6}, swp_perms([1,2,3], 2))
    }.

swp5_5_perms_case_1_test_() ->
%41 120
    {
      "swp perms of '[1,2,3,4,5] and 2' must yield in '{41, 120}'", 
      ?_assertEqual({41,120}, swp_perms([1,2,3,4,5], 5))
    }.

swp_1000_94_perms_case_7_test_() ->
    {
      "swp perms of '[1..1000] and 2' must yield in '{375743556, 19585936}'", 
      ?_assertEqual({375743556, 19585936}, 
		    swp_perms(lists:seq(1, 1000), 94))
    }.

swap_2500_test_() ->
    {
      "N Swap Permutations '[1..2500], 2500' must halt", 
      ?_assertEqual({2501, 572}, swp_perms(lists:seq(1, 2500), 2500))
    }.

swp_perms_1_to_19_4k_test_() ->
    {
      "swp perms of '[1..19] and 4' must yield in '{5, 5}'",
      ?_assertEqual({5,5}, swp_perms(lists:seq(1, 19), 4))
    }.

swp_perms(L, K) ->
    S1 = length(nas(L, K)),
    S2 = ski(L, K),
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

ski(L, K) when K > length(L) ->
    0;
ski(L, K) ->
    M = length(L),
    ski(M, K, 1, M).

ski(_M, Mi, Mi, S) ->
    S;
ski(M, Mi, I, S) ->
    ski(M, Mi, I + 1, S * (M-I)).
   
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
