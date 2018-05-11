-module(solution).
-export([main/0, jmp_bns/1]).
-import(lcm, [lcm/2]).
-include_lib("eunit/include/eunit.hrl").

jmp_bns_test() ->
    ?assertEqual(12, jmp_bns([2,3,4])).

main() ->
    _ = io:fread("", "~d"),
    Bunnies = read_nbunnies(),
    io:fwrite("~w~n", [jmp_bns(Bunnies)]),
    true.

jmp_bns(L)->
    jmp_bns(L, 1).
jmp_bns([], NearestPoint) ->
    NearestPoint;
jmp_bns([J1th,J2th|T], 1) ->
    jmp_bns(T, lcm:lcm(J1th,J2th));
jmp_bns([Jth|T], LNp) ->
    jmp_bns(T, lcm:lcm(Jth, LNp)).

read_nbunnies() ->
    case io:get_line("") of 
	eof ->
	    ok;
	Line ->
	  lists:map(fun erlang:list_to_integer/1,string:tokens(Line, "\r\n\t "))
    end.
