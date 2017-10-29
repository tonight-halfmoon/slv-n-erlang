%%%-------------------------------------------------------------------
%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2017, 
%%% @doc
%%%

%%% Generate all pairs from a given list without duplicate

%%% @end
%%% On : 29 Oct 2017 by  <rosemary@SCUBA>
%%%-------------------------------------------------------------------
-module(ppairs).
-export([pps/1]).
-include_lib("eunit/include/eunit.hrl").

pps([]) ->
    [[]];
pps(L) ->
    [[H,H2]|| H <- L, H2 <- L--[H]].


all_pairs_of_2500_elem_list_test_() ->
    {
      "", 
      ?_assertMatch(L when 6247500 == length(L), pps(lists:seq(1,2500)))}.

all_pairs_of_5_elem_list_test_() ->
    {
      "", 
      ?_assertMatch(L when 20 == length(L), pps(lists:seq(1,5)))}.

all_pairs_of_6_elem_list_test_() ->
    {
      "", 
      ?_assertMatch(L when 30 == length(L), pps(lists:seq(1,6)))}.
