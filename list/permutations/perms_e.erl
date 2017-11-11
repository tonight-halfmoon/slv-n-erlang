%%%-------------------------------------------------------------------
%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2017, 
%%% @doc
%%% Generate Perumtations of a list elementary
%%% @end
%%% On the 7th of November 2017 
%%%-------------------------------------------------------------------
-module(perms_e).
-export([start/1, permute/1]).
-include_lib("eunit/include/eunit.hrl").

start(L) ->
    spawn(?MODULE, permute, [L]).

permute([]) ->
    [[]];
permute([X]) ->
    [[X]];
permute([X,Y]) ->
    [[X,Y],[Y,X]];
permute(L) when is_list(L) ->
    permute(L, L, []).

permute(L, [X|T], Tents) ->
    permute(L, T, [
		   lists:map(fun(Y) -> conq(X, Y) end, permute(L--[X]))
		   |Tents
		  ]);
permute(_, [], Permutations) ->
    Permutations.

conq(_X, []) ->
    [];
conq(X, [H|T]) when is_list(H) ->
    [conq(X, H)|conq(X, T)];
conq(X, [H|Y]) when not is_list(H) ->
    [X|[H|Y]].

conq_t(_X, []) ->
    [];
conq_t(X, L) ->
    conq_t(X, L, []).

conq_t(X, [H|T], C) when is_list(H) ->
    conq_t(X, T, [conq_t(X, H)|C]);
conq_t(X, [H|Y], C) when not is_list(H) ->
    [[X|[H|Y]]|C];
conq_t(_X, [], C) ->
    C.

permute_empty_test_() ->
    {
      "Permute an Empty list yields into an empty list",
      ?_assertEqual([[]], permute([]))
    }.

permute_1_test_() ->
    ?_assertEqual([[1]], permute([1])).

permute_2_test_() ->
    ?_assertEqual([[1,2],[2,1]], permute([1,2])).

perms_r2_test_() ->
    ?_assertEqual([[2,1],[1,2]], permute([2,1])).

permute_10elem_list_test_() ->
    {
      "Permute 10-element list must pass",
      ?_assertMatch(_ when true, permute(lists:seq(1,10)))
    }.

%% permute_11elem_list_test_() ->
%%     {
%%       "Permute 11-element list must pass",
%%       ?_assertMatch(L when 11 == length(L), permute(lists:seq(1,11)))
%%     }.

%% permute_2500elem_list_test_() ->
%%     {
%%       "Permute 2500-element list must pass",
%%       ?_assertMatch(Pid when is_pid(Pid), permute(lists:seq(1,2500)))
%%     }.
