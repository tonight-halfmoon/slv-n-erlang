-module(remove_nth_elem2).
-include_lib("eunit/include/eunit.hrl").
-export([rm_nth/2]).

rm_nth([], _) ->
    [];
rm_nth(_, Nth) when not(is_integer(Nth)) ->
    io:format("Expect Nth of type integer~n");
rm_nth(Input, _) when not(is_list(Input)) ->
    io:format("Expect Input of type List~n");
rm_nth(List, Nth) when 0 > Nth ->
    List;
rm_nth(List, Nth) when length(List) =< Nth ->
    List;
rm_nth(Input, Nth) ->
    rm_nth(Input, Nth, 0, []).

rm_nth([_|T], Nth, Nth, Output) ->
    Output ++ T;
rm_nth([H|T], Nth, Curr_indx, Output) ->
    rm_nth(T, Nth, Curr_indx +1,  Output ++ [H]).

run_unit_test() ->
    test_rm_nth(fun rm_nth/2, [a,b,c], -1, [a,b,c]),
    test_rm_nth(fun rm_nth/2, [],-1,[]),
    test_rm_nth(fun rm_nth/2, [],0,[]),
    test_rm_nth(fun rm_nth/2, [a,b,c,d], 0, [b,c,d]),
    test_rm_nth(fun rm_nth/2, [a,b,c,d], 3, [a,b,c]),
    test_rm_nth(fun rm_nth/2, [a],-1,[a]),
    test_rm_nth(fun rm_nth/2, [a,b,c],-1,[a,b,c]),
    test_rm_nth(fun rm_nth/2, [a,b,c],4,[a,b,c]),
    test_rm_nth(fun rm_nth/2, 0, 0, ok),
    test_rm_nth(fun rm_nth/2, [1,2], [d], ok).

test_rm_nth(F, Input_list, Input_nth, Expctd_output) ->
    Actual_output = F(Input_list, Input_nth),
    ?assertEqual(Expctd_output, Actual_output).
