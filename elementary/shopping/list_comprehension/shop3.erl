-module(shop3).
-export([total/1]).
-import(lists, [sum/1]).

total(L) ->
    lists:sum([shop:cost(A)*B || {A,B} <- L]).
