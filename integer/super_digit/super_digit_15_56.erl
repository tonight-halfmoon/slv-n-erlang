-module(super_digit_15_56).
-export([main/0]).
-include_lib("eunit/include/eunit.hrl").

main() ->
    {ok, [N,K]} = io:fread("", "~d~d"),
    io:fwrite("~p~n", [super_digit(N,K)]),
    true.

super_digit(N, _) when $0 =< N andalso N =< $9 ->
    N;
super_digit(N, K) ->
    S = rem_(N,9) * K,
    super_digit(div10(S), 1, s_digit(S)).

super_digit(0, _, S)->
    S;
super_digit(N, 1, S)->
    super_digit(div10(N), 1, s_digit(S)).

s_digit(X) ->
    s_digit(X, 0, 0).

s_digit(N, _, _) when $0 =<N andalso N =< $9 ->
    N;
s_digit(0, Rem, D) ->
    Rem+D;
s_digit(X, Rem, D)  ->
    s_digit(div10(X), rem10(X), Rem+D).

rem10(X) -> rem_(X,10).
rem_(X,Y) -> X rem Y.
div10(X) -> div_(X, 10).
div_(_, 0) -> undefined;
div_(_, 1) -> 1;
div_(X,Y) -> trunc(X / Y).

super_digit_1_test_() -> 
    {"Super digit of '9875' is '2'", ?_assertEqual(2, super_digit(9875, 1))}.

super_digit_148_3_test_() -> 
    {"Super digit of '148148148' is '3'", ?_assertEqual(3, super_digit(148, 3))}.

super_digit_testcase5_test_() -> 
    {"Super digit of testcase_5 is '5'", ?_assertEqual(5, super_digit(3546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736, 100000))}.

super_digit_testcase4_test_() ->
    {"Super digit of testcase_4 is '3'", ?_assertEqual(3, super_digit(861568688536788, 100000)) }.
    

super_didit_5_test() -> ?assertEqual(2, super_digit(3546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736, 1)).
 
psuperd_4_20k_test() -> ?assertEqual(6, super_digit(861568688536788, 20)).

psuperd_5_3k_test() -> ?assertEqual(4, super_digit(3546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736, 3)).
