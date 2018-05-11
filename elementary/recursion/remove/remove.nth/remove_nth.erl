-module(remove_nth_elem).
-include_lib("eunit/include/eunit.hrl").
-export([rm/2]).

run_unit_test() ->
    test_rm_nth(fun rm/2, [a,b,c,d], 0, [b,c,d]),
    test_rm_nth(fun rm/2, [a,b,c,d], 3, [a,b,c]),
    test_rm_nth(fun rm/2, [a,b,c,d], 1, [a,c,d]).

test_rm_nth(F, Input_list, Input_nth, Expctd_output) ->
    Actual_output = F(Input_list, Input_nth),
    ?assertEqual(Expctd_output, Actual_output).

rm([], _) ->
    [];
rm(List, Nth) ->
    ?assert(is_integer(Nth)),
    ?assert(0 =< Nth),
    ?assert(is_list(List)),
    ?assert(length(List) > Nth),
    rm(List, Nth, 0, []).

rm([], _, _, Output) ->
    Output;
rm(List, Nth, CurrIndx, Output)
  when length(List) > 0 ->
    case CurrIndx =:= Nth of 
	true -> 
	    [_|T] = List,
	    rm(T, Nth, CurrIndx + 1, Output)
		;
	false ->
	    [H|T] = List, 
	    rm(T, Nth, CurrIndx + 1, Output ++ [H])
    end.
