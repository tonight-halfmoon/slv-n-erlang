-module(dvsb_by3).
-export([dvsb_by3/1]).
-import(dvsb, [dvsbl_by/2]).
-import(sdi, [sdi/1]).
-include_lib("eunit/include/eunit.hrl").

dvsb_by3(0) ->
    true;
dvsb_by3(N) when N =< 9 ->
    dvsb:dvsbl_by(N, 3);
dvsb_by3(N) ->
    dvsb:dvsbl_by(sdi:sdi(N), 3).


n_124524_divisible_by_3_test_() ->
    {"124524 is divisible by 3", ?_assertEqual(true, dvsb_by3(124524))}.

n_1212582439_divisible_by_3_test_() ->
    {"1212582439 is not divisible by 3", ?_assertEqual(false, dvsb_by3(1212582439))}.
