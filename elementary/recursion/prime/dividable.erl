-module(divisible).
-export([divby/2]).

divby(X,Y) ->
    Z = X rem Y,
    io:format("~wX rem~w Y is~w~n ", [X, Y, Z]),
   if X rem Y =:= 0 ->
	   true;
      X rem Y /=0 ->
	   false
   end.
