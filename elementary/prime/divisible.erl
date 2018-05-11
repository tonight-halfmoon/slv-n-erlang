-module(divisible).
-export([divby/2]).

divby(X,Y) ->
   if X rem Y =:= 0 ->
	   true;
      X rem Y /=0 ->
	   false
   end.
