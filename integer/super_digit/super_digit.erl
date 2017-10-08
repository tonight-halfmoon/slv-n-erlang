-module(super_digit).
-export([main/0]).
-include_lib("eunit/include/eunit.hrl").
-import(digits, [digits/1]).

super_digit_1_test_() -> 
    {"Super digit of '9875' is '2'", ?_assertEqual(2, super_digit(9875, 1))}.

super_digit_148_3_test_() -> 
    {"Super digit of '148148148' is '3'", ?_assertEqual(3, super_digit(148, 3))}.

super_digit_testcase5_test_() -> 
    {"Super digit of testcase_5 is '5'", ?_assertEqual(5, super_digit(3546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736, 100000))}.

super_digit_testcase4_test_() ->
    {"Super digit of testcase_4 is '3'", ?_assertEqual(3, super_digit(861568688536788, 100000)) }.

main() ->
    {ok, [N, K]} = io:fread("", "~d~d"),
    io:fwrite("~p~n", [super_digit(N, K)]),
    true.

super_digit(N, K) ->
    Duplicated = lists:flatten(lists:duplicate(K, digits:digits(N))),
    sum(Duplicated).
   
sum(L) when is_list(L) ->
    %io:fwrite("L is a list: ~w~n", [L]),
    S = lists:sum(L),
    %io:fwrite("Sum of L: ~w~n", [S]),
    sum(S);
sum(N) when 0 =< N andalso N =< 9  ->
    %io:fwrite("N is less than 10: ~w~n", [N]),
    N;
sum(Integer) ->
    %io:fwrite("Integer: ~w~n", [Integer]),
    S = digits:digits(Integer),
    sum(S).
    
super_didit_5_test() ->
    ?assertEqual(2, super_digit(3546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736, 1)).

psuperd_4_20k_test() -> 
    ?assertEqual(6, super_digit(861568688536788, 20)).

psuperd_5_3k_test() -> 
    ?assertEqual(4, super_digit(3546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736, 3)).
