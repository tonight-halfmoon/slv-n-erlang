-module(lib_misc).
-export([for/3, times1000/2]).

for(Max, Max, F) ->
    [F(Max)];
for(I, Max, F) ->
    [F(I)|for(I+1, Max, F)].

times1000(X, Y) ->
    for(X, Y, fun(I) -> I * 1000 end).

