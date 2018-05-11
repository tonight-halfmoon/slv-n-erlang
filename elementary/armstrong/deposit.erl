-module(deposit).
-export([deposit/2]).

deposit(Who, Money) ->
    Old = lookup(Who),
    New = Old + Money,
    insert(Who, New),
    New.

lookup(Who) ->
    44.

insert(Who, New) ->
    ok.
