%%% @author Amado Elga <rosemary@scuba>
%%% @copyright (C) 2018, Amado Elga
%%% @doc
%%%
%%% @end
%%% Created :  1 Sep 2018 by Amado Elga <rosemary@scuba>

-module(functions_as_results).

-export([times/1]).
-export([doubleAll/1]).

times(X) ->
    fun(Y) -> X * Y end.

doubleAll(Xs) ->
    map(times(2), Xs).

map(_F, []) ->
    [];
map(F, [X|Xs]) ->
    [F(X)| map(F, Xs)].
