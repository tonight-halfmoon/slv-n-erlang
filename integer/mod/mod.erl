-module(mod).
-include_lib("eunit/include/eunit.hrl").
-export([mod/2]).

an_integer_mod_the_same_except_zero_equals_zero_test() ->
    ?assertEqual(0, mod(324234123, 324234123)).

any_integer_mod_one_equals_zero_test() ->
    ?assertEqual(0, mod(213123,1)).

one_mod_zero_is_undefined_test() ->
    ?assertEqual(undefined, mod(1,0)).
one_mod_any_integer_except_one_equals_one_test() ->
    ?assertEqual(1, mod(1, 12)).

zero_mod_one_equals_zero_test() ->
    ?assertEqual(0, mod(0,1)).

mod_neg512_and_neg12_must_yield_to_neg8_test() ->
    ?assertEqual(-8, mod(-512,-12)).
mod_neg512_and_12_must_yield_to_4_test() ->
    ?assertEqual(4, mod(-512,12)).
mod_512_and_12_must_yield_to_8_test() ->
    ?assertEqual(8, mod(512,12)).

mod_neg512_and_neg12939393_must_yield_to_neg512_test() ->
    ?assertEqual(-512, mod(-512,-12939393)).

mod_5120092819399393_and_212390129292_must_yield_to_3972557149_test() ->
    ?assertEqual(3972557149, mod(5120092819399393, 212390129292)).

mod(0,0) ->
    undefined; %% The divisor must not be 0.
mod(_,0) ->
    undefined; %% The divisor must not be 0.
mod(0,_) ->
    0;
mod(X,X) ->
    0; 
% mod(1, 1) -> 0;
% mod(1, _) -> 1;
mod(_, 1) ->
    0;
mod(X, Y) ->
    ((X rem Y) + Y) rem Y.
