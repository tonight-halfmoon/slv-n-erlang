-module(remove_except). %%%%%%%%%  6thFeb16; Jeddah, SA      %%%%%%%%%
-include_lib("eunit/include/eunit.hrl").
-export([rm_except/2]).

run_unit_test() -> 
    test_rm_except(fun rm_except/2,[a,b,c], -1, [a,b,c]),
    test_rm_except(fun rm_except/2,[], -1, []),
    test_rm_except(fun rm_except/2,[], 0, []),
    test_rm_except(fun rm_except/2,[a,b,c,d], 0, [a]),
    test_rm_except(fun rm_except/2,[a,b,c,d], 3, [d]),
    test_rm_except(fun rm_except/2,[a], -1, [a]),
    test_rm_except(fun rm_except/2,[a,b,c], -1, [a,b,c]),
    test_rm_except(fun rm_except/2,[a,b,c], 4, [a,b,c]),
    test_rm_except(fun rm_except/2,0,0,ok),
    test_rm_except(fun rm_except/2,[a,2,3],[er],ok).

test_rm_except(F, Input_List, Input_Nth, Expctd_output) ->
    ActualOutput = F(Input_List, Input_Nth),
    ?assertEqual(Expctd_output, ActualOutput).

rm_except([], _) ->
    [];
rm_except(_, Nth) when not(is_integer(Nth))->
    io:format("Expect Nth of type Integer~n");
rm_except(Input, _) when not(is_list(Input)) ->
    io:format("Expect Input of type List~n");
rm_except(List, Nth) when 0 > Nth ->
    List;
rm_except(List, Nth) when length(List) =< Nth ->
    List;
rm_except(Input, Nth) ->
    	    rm_except(Input, Nth, 0).

rm_except([H|_], Nth, Nth) -> 
    [H];
rm_except([_|T], Nth, Curr_indx) ->
    rm_except(T, Nth, Curr_indx + 1).
