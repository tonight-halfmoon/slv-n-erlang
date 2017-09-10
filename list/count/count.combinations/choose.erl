-module(choose).
-export([choose/2, choose_tail/2]).
-include_lib("eunit/include/eunit.hrl").

%%% Simple divide and conquer algorithm's implementation

choose(N, R) ->
    choose_tail(N, R).

choose_tail(N, R) ->
    choose_tail(N, R, 0, 1).

choose_tail(N, N, _Acc1, Acc2) ->
    Acc2;
choose_tail(0, _, Acc1, _Acc2) ->
    Acc1;
choose_tail(_N, 0, _Acc1, Acc2) ->
    Acc2;
choose_tail(N, R, Acc1, _Acc2) when N < R ->
    Acc1;
choose_tail(N, R, Acc1, Acc2) when N > R  ->
    choose_tail(N -1, R -1, Acc1 + Acc2, Acc2 + choose_tail(N-1, R)).

%%% no tail
%choose(0, _) ->
%    0;
%choose(_, 0) ->
%    1;
%choose(N,N) ->
%    1;
%choose(N, R) when N < R ->
%    0;
%choose(N, R) ->
%    choose(N-1, R-1) + choose(N-1, R).

choose_n_r_test_() ->
    {"'choose(9,2)' yields to '36'", ?_assertEqual(36, choose(9, 2))}.

choose_100_2_test_() ->
    {"'choose(100,2)' yields to '4950'", ?_assertEqual(4950, choose(100, 2))}.

choose_99_2_test_() ->
    {"'choose(99,2)' yields to '4851'", ?_assertEqual(4851, choose(99, 2))}.

choose_98_2_test_() ->
    {"'choose(98,2)' yields to '4753'", ?_assertEqual(4753, choose(98, 2))}.

choose_97_2_test_() ->
    {"'choose(97,2)' yields to '4656'", ?_assertEqual(4656, choose(97, 2))}.

choose_100_3_test_() ->
    {"'choose(100, 3)' yields to '161700'", ?_assertEqual(161700, choose(100, 3))}.

choose_370_2_test_() ->
    {"'choose(370,2)' yields to '68265'", ?_assertEqual(68265, choose(370, 2))}.

