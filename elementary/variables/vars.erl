-module(vars).
-export([what/0]).

what() ->
    Var = one,
    Double = Var *2.
