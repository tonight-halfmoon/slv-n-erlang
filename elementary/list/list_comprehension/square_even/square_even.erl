-module(square_even).
-export([square_even/1]).

square_even(Set) ->
    [Next * Next || Next <- Set, Next rem 2 =:= 0].
