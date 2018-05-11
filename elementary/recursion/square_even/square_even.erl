-module(square_even).
-export([square_even/1]).

square_even([])->
    [];
square_even([H|T]) ->
    if H rem 2 =:= 0 -> 
	    [H*H] ++ square_even(T);
       true ->
	       square_even(T)
end.
		
