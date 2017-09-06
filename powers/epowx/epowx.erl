-module(epowx).
-export([main/0, epowx/1]).
-include_lib("eunit/include/eunit.hrl").

%%% Evaluate e^x by computing the sum of the first 10 terms of the series. 
%%% Future work provide a proper API for giving a customisable number of terms.

epowx_test_()->
    [
     {"Expect 'e^20.0000' to evaluate as '2423600.1887'", ?_assertEqual(2423600.1887, epowx(20.0000))}
    ].

epowx(X) ->
    binary_to_float(float_to_binary(epowx(X, 0, 10, 0), [{decimals, 4}])).

epowx(_, T9, T9, Eval) ->
    Eval;
epowx(X, T0, T9, Eval) ->
    Pow = pow(X, T0),
    Fact = fact(T0),
    R = Pow/Fact,
    epowx(X, T0 + 1, T9,  R + Eval).

pow(_, 0) ->
    1.0;
pow(Base, 1) ->
    Base;
pow(Base, T) when T >= 2->
    pow(Base, 1, T, Base).

pow(_, T, T, Acc) ->
    Acc;
pow(Base, I, T, Acc) ->
    pow(Base, I + 1 , T, Base * Acc).

fact(N)->
    fact(N, 1).

fact(0, Acc) ->
    Acc;
fact(1, Acc) ->
    Acc;
fact(N, Acc) when N > 1 ->
    fact(N - 1, N * Acc).

read_list(N) ->
    read_list(N, 0, []).

read_list(0, _, L) ->
    L;
read_list(N, N, L) ->
    L;
read_list(N, I, L) ->
    {ok, [X]} = io:fread("", "~f"),
    read_list(N, I+1, lists:append([L, [X]])).

main() ->
    {ok, [N]} = io:fread("", "~d"),
    Xs = read_list(N),
    eval_all(Xs),
    true.

eval_all([]) -> 
    ok;
eval_all([X|Xs]) ->
    print(epowx(X)),
    eval_all(Xs).

print(Val) ->
    io:format("~.7g~n", [Val]).

