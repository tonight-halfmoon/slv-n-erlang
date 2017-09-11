-module(cpdc).
-export([cpdc/1]). 
-import(choose, [choose/2]).
-include_lib("eunit/include/eunit.hrl"). 
-include("./include/testcase_input02_02.hrl").

%%% Problem Statement
%%% Count the total number of pairs of indices (i, j) where Ai = Aj and i =/= j.

%%% Solution utilises divide and conquer with spatial counting

cpdc_test_() ->
    {"must pass testcase02_02", ?_assertEqual(9948, cpdc(?testcase_input02_02()))}.

cpdc(L) ->
    cpdc(L, maps:new()).

cpdc([], C) ->
    Fun = fun(_K, V, AccIn) -> case V of 1 -> AccIn; _ -> AccIn + choose:choose(V,2) * 2 end end,
    maps:fold(Fun, 0, C);

cpdc([H|T], C) ->
    CurrOcc = maps:get(H, C, 0),
    case CurrOcc of
	0 ->
	    NewC = maps:put(H, 1, C);
	_ ->
	    NewC = maps:update(H, CurrOcc + 1, C)
    end,
    cpdc(T, NewC);
cpdc(L, C) ->
    {Fh, Sh} = lists:split(trunc(length(L) / 2), L),
    cpdc(Fh, C) + cpdc(Sh, C).
