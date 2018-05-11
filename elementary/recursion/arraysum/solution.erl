-module(solution).
-export([main/0]).

main()->
    {ok, [N]} = io:fread("", "~d"),
    Ints = readn(N), 
    io:format("Ints~w~n", [Ints]),
    io:format("~p~n",[sum(Ints)]). 

readn(N)->
    readn(0,N,[]).
readn(N,N, L) -> L;
readn(Next, N, L) ->
    {ok, [In]} = io:fread("","~d"),
    readn(Next+1, N, [In|L]).

sum(L) ->
     sum(L, 0).
sum([], Acc) -> Acc;
sum([H|T], Acc) -> sum(T, Acc + H).
    
