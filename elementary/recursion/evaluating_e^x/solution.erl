-module(solution).
-export([main/0, fact/1]).
-include_lib("eunit/include/eunit.hrl").
%-include_lib("stdlib/include/assert.hrl").

eval_epowx_test()->
    ?assertEqual(2423600.1887, eval_epowx(20.0000)).

main() ->
    {ok, [N]} = io:fread("", "~d"),
    Xs = read_list(N),
    eval_all(Xs),
    %io:format("~.7g~n", [eval_epowx(5.0000)]),
    true.

% X^0/0! + x^1/1!
%xpow(X,Term) Term:[0, 10]
% fact(Term)
eval_all([]) -> ok;
eval_all([X|Xs]) ->
    print_output(eval_epowx(X)),
    eval_all(Xs).
print_output(Val) ->
    io:format("~.7g~n", [Val]).
eval_epowx(X) ->
    epowx(X, 0, 10, 0).

epowx(_, T9, T9, Eval) ->
    Eval;
epowx(X, T0, T9, Eval) ->
    Pow =pow(X, T0),
    Fact = fact(T0),
    %assert(Fact =/= 0),
    R = Pow/Fact,
    %io:fwrite("Term:~w~n", [T0]),
    %io:fwrite("~wPow~w: ~w~n", [X,T0,Pow]),
    %io:fwrite("Fact: ~w~n", [Fact]),
    %io:fwrite("R:~w~n", [R]),
    epowx(X, T0+1, T9,  R+ Eval).

pow(_, 0) ->
    1.0;
pow(Base, 1) ->
    Base;
pow(Base, T) when T >= 2->
    pow(Base, 1, T, Base).
pow(_, T, T, Acc) ->
    Acc;
pow(Base, I, T, Acc) ->
    pow(Base, I+1 , T, Base *Acc).

fact(N)->
    fact(N, 1).
fact(0, Acc) ->
    Acc;
fact(1, Acc) ->
    Acc;
fact(N, Acc) when N > 1 ->
    fact(N-1, N * Acc).

read_list(N) ->
    read_list(N, 0, []).
read_list(0, _, L) ->
    L;
read_list(N, N, L) ->
    L;
read_list(N, I, L) ->
    {ok, [X]} = io:fread("", "~f"),
    read_list(N, I+1, lists:append([L, [X]])).
