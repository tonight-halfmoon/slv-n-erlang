-module(shop).
-export([cost/1, total/1]).

cost(oranges) -> 5;
cost(newspaper) ->
    8;
cost(apples) ->
    2;
cost(pears) ->
    9;
cost(milk) ->
    7.


total([{What, N}| T]) ->
   shop:cost(What) * N + total(T);
total([]) ->
    0.

sum(L) ->
    sum(L,0).

sum([],N)->
    N;
sum([H|T],N) ->
    sum(T, H+N).

