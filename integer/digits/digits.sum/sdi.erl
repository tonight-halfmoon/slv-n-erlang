-module(sdi).
-export([sdi/1]).
-include_lib("eunit/include/eunit.hrl").

sdi(N) when 0 > N ->
    sdi(abs(N));
sdi(N) when N >= 0 andalso N =< 9 ->
    N;
sdi(N) when not is_integer(N) ->
    <<"N must be an integer">>;
sdi(N) ->
    sdi(N, 0).

sdi(0, Sum)->
    Sum;
sdi(N, Sum)->
    sdi(trunc(N/10), trunc(N rem 10)+Sum).

integer_12345_digits_sum_test_() ->
    {"Digitis sum of integer 12345 is 15", ?_assertEqual(15, sdi(12345))}.

integer_0_digits_sum_test_() ->
    {"", ?_assertEqual(0, sdi(0))}.
