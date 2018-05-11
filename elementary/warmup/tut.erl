-module(tut).
-export([double/1]).
-export([convert/2]).


double(X) ->
    2 * X.


convert(M, inch) ->
    M/2.54;
convert(N, centimeter) ->
    N * 2.54.

