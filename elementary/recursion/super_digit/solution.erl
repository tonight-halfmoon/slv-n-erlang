-module(solution).
-export([main/0]).
-include_lib("eunit/include/eunit.hrl").

super_digit_test() ->
    ?assertEqual(3, p_superd(148, 3)).

main() ->
    {ok, [N,K]} = io:fread("", "~d~d"),
    io:fwrite("~p~n", [p_superd(N,K)]),
    true.

super_digit(X) ->
    super_digit(X, 0, 0).

super_digit(N, _, _) when $0 =< N andalso N =< $9 ->
    N;
super_digit(0, Rem, Super) ->
    Rem+Super;
super_digit(X, Rem, Super)  ->
    super_digit(div10(X), rem10(X), Rem+Super).

p_superd(N, K) ->
    p_superd(N, K, p(N,K)).
p_superd(_, _, P) when $0 =< P andalso P =< $9 ->
    P;
p_superd(0, _, P) ->
    P;
p_superd(_N, K, P)->
   io:fwrite("P: ~p~n", [P]),
   p_superd(div10(P), K, super_digit(P)).
 
p(N,K) ->
    p(N, K, 0, 0).
p(_, K, K, S) ->
    S;
p(N, K, I, S) ->
   io:fwrite("S: ~p~n", [S]),
   p(N, K, I+1, super_digit(N) + S).

rem10(X) -> rem_(X,10).
rem_(X,Y) -> X rem Y.
div10(X) -> div_(X, 10).
div_(_, 0) -> undefined;
div_(_, 1) -> 1;
div_(X,Y) -> trunc(X / Y).
