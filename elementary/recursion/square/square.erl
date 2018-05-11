-module(square).
-export([square/1]).

square([]) ->
    [];
square([H|T]) ->
    [H*H]++ square(T). 
