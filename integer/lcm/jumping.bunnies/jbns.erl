-module(jbns).
-export([main/0, jmp_bns/1]).
-import(lcm, [lcm/2]).
-include_lib("eunit/include/eunit.hrl").

%%% Problem Statement: Find out the nearest point where all bunneis can meet while each jumps for a different range.

jmp_bns(L)->
    jmp_bns(L, 1).
jmp_bns([], NearestPoint) ->
    NearestPoint;
jmp_bns([J1th,J2th|T], 1) ->
    jmp_bns(T, lcm:lcm(J1th,J2th));
jmp_bns([Jth|T], LNp) ->
    jmp_bns(T, lcm:lcm(Jth, LNp)).

main() ->
    _ = io:fread("", "~d"),
    Bunnies = read_nbunnies(),
    io:fwrite("~w~n", [jmp_bns(Bunnies)]),
    true.

read_nbunnies() ->
    case io:get_line("") of 
	eof ->
	    ok;
	Line ->
	  lists:map(fun erlang:list_to_integer/1,string:tokens(Line, "\r\n\t "))
    end.


jmp_bns_test() ->
    ?assertEqual(12, jmp_bns([2,3,4])).

jmp_bns_5148_test() ->
    ?assertEqual(5148, jmp_bns([3,4,6,78,99])).

jmp_bns_testcase7_test() ->
    ?assertEqual(890969009638765049, jmp_bns([997,991,983,977,971,967])).

jmp_bns_testcase10_test() ->
    ?assertEqual(3, jmp_bns([1,3])).

jmp_bns_testcase8_test() ->
    ?assertEqual(126459568506372769, jmp_bns([157, 151, 149, 139, 137, 131, 127, 113])).
