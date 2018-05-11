-module(all).
-export([main/0]).
-include_lib("eunit/include/eunit.hrl").

jmp_bns_test() ->
    ?assertEqual(12, jmp_bns([2,3,4])).
jmp_bns_5148_test() ->
    ?assertEqual(5148, jmp_bns([3,4,6,78,99])).
jmp_bns_testcase7_test() ->
    ?assertEqual(890969009638765049, jmp_bns([997,991,983,977,971,967])).

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
    jmp_bns(T, lcm(J1th,J2th));
jmp_bns([Jth|T], LNp) ->
    jmp_bns(T, lcm(Jth, LNp)).

read_nbunnies() ->
    case io:get_line("") of 
	eof ->
	    ok;
	Line ->
	  lists:map(fun erlang:list_to_integer/1,string:tokens(Line, "\r\n\t "))
    end.

lcm(X,X) ->
    X;
lcm(X,Y) ->
   mul(div_(X, gcd(X,Y)),Y).

mul(X,Y) ->
    X*Y.

div_(_,0) ->
    undefined;
div_(X,Y) ->
    trunc(X/Y).

gcd(X,0) ->
    X;
gcd(X,Y) ->
    gcd(Y, mod(X,Y)).

mod(X,X) ->
    0;
mod(_, 1) ->
    0;
mod(X, Y) ->
    X rem Y.%((X rem Y) + Y) rem Y.
