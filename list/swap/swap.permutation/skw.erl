%%%-------------------------------------------------------------------
%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2017, 
%%% @doc
%%%
%%% @end
%%% Created : 21 Oct 2017 by  <rosemary@SCUBA>
%%%-------------------------------------------------------------------
-module(skw).
-author('rosemary@SCUBA').
-export([skw/2]).
-include_lib("eunit/include/eunit.hrl").
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------



skw_test_() ->
    { 
      "swp perms of '[1,2,3] and 2' must yield in '{3, 6}'", 
      ?_assertEqual(6, skw([1,2,3], 2))
    }.


%%% Count k-Swaps
skw(L, K) -> 
    skw(L, K, 1).

skw([], _, S) ->
    S;
skw([X], K, S) ->
    io:fwrite("skw[X]; S: ~w~n", [S]),
    ski([X], K) * S;
skw([X,Y], K, S) ->
    io:fwrite("skw[X, Y]; S: ~w~n", [S]),
    ski([X,Y], K) * S;
skw([X,Y,Z], K, S) ->
    io:fwrite("skw[X,Y,Z]; : ~w~n", [[X,Y,Z]]),
    io:fwrite("skw[X,Y,Z]; S: ~w~n", [S]),
    ski([X,Y,Z], K) * S;
skw(L, K, S) ->
    io:fwrite("skw L: ~w~n", [L]),
    io:fwrite("S: ~w~n", [S]),
    {Left, Right} = split(L),
    skw(Left, K, S * skw(Right, K)).

ski(L, K) when K > length(L) ->
    0;
ski(L, K) ->
    M = length(L),
    ski(M, K, 1, M).

ski(_M, Mi, Mi, S) ->
    S;
ski(M, Mi, I, S) ->
    ski(M, Mi, I + 1, S * (M-I)).

split([]) ->
    {[],[]};
split([X]) ->
    {[X], []};
split([H|T]) -> 
    split(T, [H]).

split(L, R) when length(L) =:= length(R);
		 length(L) =:= length(R) + 1 ->
    {R, L};
split([H|T], R) ->
    split(T, [H|R]).
