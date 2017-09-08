-module(dvdb).
-export([dvdbl_by/2]).
-include_lib("eunit/include/eunit.hrl").

%%% Dividing 0 by 0 is not possible. This is diffirent than the problem of divisibility.
%%dvdbl_by(0, 0) ->
%%    false;
dvdbl_by(_, 0) ->
    undefined;
dvdbl_by(0, _) ->
    true;

dvdbl_by(X, Y) when not is_integer(X); not is_integer(Y) ->
    <<"Both parameters must be integers">>;
dvdbl_by(X, Y) ->
    if X rem Y =:= 0 ->
	    true;
       X rem Y /= 0 ->
	   false
    end.

b_0_divisible_by_0_test_() ->
    {"0 cannot be divided by 0.", ?_assertEqual(undefined, dvdbl_by(0, 0))}.

b_0_divisible_by_n_test_() ->
    {"0 is dividable by any integer", ?_assertEqual(true, dvdbl_by(0, 9))}.
b_16_divisible_by_4_test_() ->
    {"16 is dividable by 4", ?_assertEqual(true, dvdbl_by(16,4))}.
