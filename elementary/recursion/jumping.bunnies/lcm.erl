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

lcm(0,_) ->
    0;
lcm(_,0) ->
    0;
%lcm(0,0)->0; %% implicitely defined with two patterns above
lcm(X,X) ->
    abs(X);
lcm(Xsigned,Ysigned) ->
    X = abs(Xsigned),
    Y = abs(Ysigned),
    %io:fwrite("X: ~w; Y: ~w)~n", [X,Y]),
    div_( mul(X,Y), gcd:gcd(X,Y)).

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
