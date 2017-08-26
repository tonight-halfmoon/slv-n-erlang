-module(extract_nth).
-include_lib("eunit/include/eunit.hrl").
-export([nth/2]).

nth_must_be_integer_test() ->
    ?assertEqual({8.1, must_be_integer}, nth(8.1, [y,f,g,5,h,e,a])).
    
nth_must_not_exceed_range_found_test() ->
    ?assertEqual({8, must_not_exceed_range}, nth(8, [y,f,g,5,h,e,a])).

zero_nth_not_applicable_test() ->
    ?assertEqual({0, nth_must_be_greater_than_or_equal_to_1}, nth(0, [1])).

must_not_be_empty_list_even_zero_nth_test() ->
    ?assertEqual({0, nth_cannot_be_found_in_empty_list}, nth(0, [])).
        
n3rd_not_found_in_empty_lst_test() ->
    ?assertEqual({3, nth_cannot_be_found_in_empty_list}, nth(3, [])).

neg_n3rd_cannot_be_negative_test() ->
    ?assertEqual({-3, nth_cannot_be_negative}, nth(-3, [1])).

n3rd_found_test() ->
    ?assertEqual({g, [y,f,5,h,e,a]}, nth(3, [y,f,g,5,h,e,a])).

fvth_found_test() ->
    ?assertEqual({g, [y,f,5,h]}, nth(5, [y,f,5,h,g])).

first_found_test() ->
    ?assertEqual({g, [y,f,5,h]}, nth(1, [g,y,f,5,h])).

nth(Nth, []) ->
    {Nth, nth_cannot_be_found_in_empty_list};
nth(Nth, L) ->
    case {acceptable_nth(Nth), Nth =< length(L)} of
	{true, true} ->
	    nth(1, Nth, L, []);
	{{false, Msg}, _} ->
	    {Nth, Msg};
		{_, false} ->
	    {Nth, must_not_exceed_range}
    end.

nth(Max, Max, [H|T], R) ->
    {H, lists:append(R,T)};
nth(C, Nth, [H|T], Rst) ->
    nth(C+1, Nth, T, lists:append(Rst, [H])).

acceptable_nth(0) ->
    {false, nth_must_be_greater_than_or_equal_to_1};
acceptable_nth(Nth) ->
    case {is_integer(Nth), abs(Nth) =/= - Nth } of
	{true, true} ->
	    true;
	{false, _} ->
	    {false, must_be_integer};
	{_, false} ->
	    {false, nth_cannot_be_negative}
    end.
