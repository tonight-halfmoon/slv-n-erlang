-module(shadowed_variables).

-export([foo/0, bar/0]).


foo() ->
    X = 2,
    Bump = fun(X) ->
		   X + 1 end,	
    Bump(10).

bar() ->
    X = 3,
    Add = fun(Y) ->
		  X + Y end,
    Add(10).
