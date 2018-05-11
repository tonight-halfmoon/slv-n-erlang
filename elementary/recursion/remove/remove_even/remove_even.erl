-module(remove_even).
-export([rm_even/1]).
-include_lib("eunit/include/eunit.hrl").

run_unit_test() ->
    ?assertEqual(rm_even([1,2,3,4]), [1,3]),
    ?assertEqual(rm_even(0),ok),
    ?assertEqual(rm_even([a,2,5]),ok),
    ?assertEqual(rm_even([1,2,a]),ok),
    ?assertEqual(rm_even(a),ok).

rm_even([]) ->
    [];
rm_even(Input) ->
    case is_list(Input) of
	true ->
	    rm_even(Input, [])
		;
	false ->
	    io:format("Expect a data type of List~n")
    end.
rm_even([], Output) ->
    Output;

rm_even([H|T], Output) ->
    case is_integer(H) of 
	true ->
	    case H rem 2 =/= 0 of 
		true ->
		    rm_even(T, Output ++ [H])
			;
		false ->
		    rm_even(T, Output)
	    end;
	false ->
	    io:format("Expect list elements of type Integer~n")
    end.
