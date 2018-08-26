-module(function).
-export([double/1]).

-define(FUNC, X).
-define(TION, +X).

double(X) ->
    ?FUNC?TION.
