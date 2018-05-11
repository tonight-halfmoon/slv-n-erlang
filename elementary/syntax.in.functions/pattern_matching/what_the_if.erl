-module(what_the_if).
-export([heh_fine/0]).
-export([the_ifelse_erlang/1]).
-export([help_me/1]).

heh_fine() ->
	   if 1 =:= 1 ->
	      works
	   end,
	   if 1 =:= 2; 1 =:= 1 ->
	      works
	   end,
	   if 1 =:= 2, 1 =:= 1 ->
	      fails
	   end.

the_ifelse_erlang(N) ->
		     if N =:= 2 -> might_succeed;
		     	true -> always_does
		     end.

help_me(Animal) ->s in
		Talk = if Animal == cat -> "meow";
		       	  Animal == beef -> "mooo";
		       	  Animal == dog -> "bark";
			  Animal == tree -> "bark";
			  true -> "jhyftre"
			end,
		{Animal, "says " ++ Talk ++ "!"}.
