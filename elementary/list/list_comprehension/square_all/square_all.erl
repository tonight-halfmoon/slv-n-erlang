-module(square_all).
-export([square_all/1]).

square_all(Set) ->
    [Next * Next || Next <- Set].
