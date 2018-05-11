-module(what_the_if).
-export([heh_fine/0, oh_good/1, help_me/1, else_keefe_way/2]).

heh_fine() ->
    if 1 =:= 1 ->
	    works
    end,
    if 1 =:= 2 ; 1 =:= 1 -> works
    end,
    if 1 =:= 2, 1 =:= 1 -> fails
end.

oh_good(N) ->
    if 
	N =:= 2 ->  might_succeed ;
           true ->  always_does     %% This is Erlang's if's 'else!'
end.

help_me(Animal) ->
    Talk = if Animal == cat ->
		   "meow";
	      Animal == beef -> "mooo";
	      Animal == dog -> "bark";
	      true -> "asd"
	   end,
    {Animal, "says " ++ Talk ++ "!"}.
		   
%% Richard O'Keefe's way


else_keefe_way(X,Y) ->    
    if X > Y -> a()
     ; X =< Y -> b()
    end.
a() ->
    'x>y'.
b()->
    'x=<y'.

	
