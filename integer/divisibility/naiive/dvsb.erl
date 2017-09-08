-module(dvsb).
-export([dvsbl_by/2]).
-include_lib("eunit/include/eunit.hrl").

%%% 0 is divisible by 0. Because there exists an integer k such that 0 = k0.
%%% The real definition of divisibility has nothing to do with the remainder after division. But this is a smiple implementation 
%%% while keeping in conformance to the definition as possible..
dvsbl_by(0, _) ->
    true;
dvsbl_by(_, 0) ->
    undefined;
dvsbl_by(X, Y) when not is_integer(X); not is_integer(Y) ->
    <<"Both parameters must be integers">>;
dvsbl_by(X, Y) ->
    if X rem Y =:= 0 ->
	    true;
       X rem Y /= 0 ->
	   false
    end.

b_0_divisible_by_0_test_() ->
    {"0 is divisible by 0 because there exists an integer k such that 0 = k*0.", ?_assertEqual(true, dvsbl_by(0, 0))}.
b_0_divisible_by_n_test_() ->
    {"0 is divisible by any integer", ?_assertEqual(true, dvsbl_by(0, 9))}.
b_16_divisible_by_4_test_() ->
    {"16 is divisible by 4", ?_assertEqual(true, dvsbl_by(16,4))}.
