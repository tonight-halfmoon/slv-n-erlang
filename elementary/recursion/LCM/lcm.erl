-module(lcm).
-export([main/0,lcm/2]).
-import(gcd, [gcd/2]).
-include_lib("eunit/include/eunit.hrl").

lcm_1_1_test() ->
   ?assertEqual(1, lcm(1,1)).
lcm_0_0_test() -> 
    ?assertEqual(0, lcm(0,0)).
lcm_0_1_test() -> 
    ?assertEqual(0, lcm(0,1)).
lcm_1_0_test() -> 
    ?assertEqual(0, lcm(1,0)).
lcm_7_1_test() -> 
    ?assertEqual(7, lcm(7,1)).
lcm_1_7_test() -> 
    ?assertEqual(7, lcm(1,7)).
lcm_99_0_test() -> 
    ?assertEqual(0, lcm(99,0)).
lcm_0_99_test() -> 
    ?assertEqual(0, lcm(0,99)).
lcm_3_2_test() -> 
    ?assertEqual(6, lcm(3,2)).
lcm_99_1_test() -> 
    ?assertEqual(99, lcm(99,1)).
lcm_1_99_test() -> 
    ?assertEqual(99, lcm(1,99)).
lcm_78_7_test() -> 
    ?assertEqual(546, lcm(78,7)).
lcm_7_78_test() -> 
    ?assertEqual(546, lcm(7,78)).

lcm_any_integer_with_zero_test() -> %% ?? why does pass?
    ?assertEqual(0, lcm(5,0)).

lcm_any_neg_integer_with_zero_test() -> %% ?? why does pass?
    ?assertEqual(0, lcm(-5,0)).

%%%% ZERO is not a divisor!

jmp_bns_testcase7_test() ->
    ?assertEqual(988027, lcm(997,991)).
jmp_bns_testcase71_test() ->
    ?assertEqual(971230541, lcm(988027,983)).
jmp_bns_testcase711_test() ->
    ?assertEqual(971230541, lcm(988027,971230541)).
jmp_bns_testcase7111_test() ->
    ?assertEqual(965302379, lcm(988027,977)).
jmp_bns_testcase71111_test() ->
    ?assertEqual(937308610009, lcm(965302379,971)).
jmp_bns_testcase711111_test() ->
    ?assertEqual(906377425878703, lcm(937308610009,967)).

lcm(1,X)->
    X;
lcm(X,1)->
    X;
%lcm(0,_) ->
%    0;
%lcm(_,0) -> %% this is not absolutely correct; if it is negative then the integer itself, otherwose 0
%    0;
%lcm(0,0)->0; %% implicitely defined with two patterns above
%but it is minus-infinity!
lcm(0,0) ->
    'minus-infinity';
lcm(X,X) ->
    abs(X);
lcm(Xsigned,Ysigned) ->
    X = abs(Xsigned),
    Y = abs(Ysigned),
    %io:fwrite("X: ~w; Y: ~w)~n", [X,Y]),
    mul(div_(X,gcd:gcd(X,Y)),Y).

mul(X,Y) ->
    X*Y.

div_(_,0) ->
    undefined;
div_(X,Y) ->
    trunc(X/Y).

main() ->
    {ok, [X,Y]} = io:fread("Prompt X,Y :> ", "~d~d"),
    io:fwrite("LCM(~w,~w):= ~w~n", [X,Y, lcm(X,Y)]),
    true.
