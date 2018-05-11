-module(gcd).
-export([main/0, mod/2, gcd/2]).
-include_lib("eunit/include/eunit.hrl").

gcd_test() ->
    ?assertEqual(3 , gcd(6,3)).
gcd_3_2_test() ->
    ?assertEqual(1 , gcd(3,2)).
gcd_2_3_test() ->
    ?assertEqual(1 , gcd(2,3)).

mod_1_minus12_test() ->
    ?assertEqual(-11, mod(1,-12)).
mod_minus1_13_test() ->
    ?assertEqual(12, mod(-1,13)).
mod_minus1_minus276_minus12_test() ->
    ?assertEqual(0, mod(-276,-12)).
mod_minus1_minus276_43_test() ->
    ?assertEqual(25, mod(-276,43)).

main() ->
    {ok, {X,Y}} = io:fread("X, Y> ", "~d~d"),
    io:fwrite("gcd(~w,~w):= ~w~n", [X,Y,gcd(X,Y)]),
    true.

gcd(0,0) ->
    infinity;
gcd(X,0) ->
    abs(X);
gcd(X,X) ->
    abs(X);
%%gcd(X,Y) when X < Y -> buggy!! if two minus integers -> infinit loop
%%    gcd(Y,X);
gcd(X,Y) -> %when X > Y ->
    gcd(Y, mod(X,Y)).


mod(0,0) ->
    undefined;
mod(_,0) ->
    undefined;
mod(0,_) ->
    0;
mod(X,X) ->
    0; % mod(1,1)-> 0;
%mod(1,_) ->
%    1;
mod(_, 1) ->
    0;
mod(X, Y) ->
    ((X rem Y) + Y) rem Y.
