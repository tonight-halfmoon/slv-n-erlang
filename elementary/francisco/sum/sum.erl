-module(sum).
-export([sum/1]).


sum(Boundary) ->
    sum(1, Boundary, 0).

sum(I, B, R) when I =< B ->
    sum(I + 1, B, I + R);
sum(_I, _B, R) ->
    R.
