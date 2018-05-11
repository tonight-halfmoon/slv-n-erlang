-module(fact).
-export([fact/0, evaluate/0]).

%%% Reference Armstrong Thesis 2003; P. 57
%%% Higher Order functions : A function that returns a function and a function which takes functions as input argumets
%%% devilish ingenuity recursive functions

evaluate() ->
    receive
	{'EXIT', _} ->
	    true;
	{N, Pid} ->
	    F = fun fact/0,
	    Fact = F(),
	    Result = Fact(N),
	    Pid ! Result,
	    evaluate();
	_ ->
	    true
    end.

fact() ->
    fun(X) ->
	    G = fun(0, Acc, _F) -> Acc;
		   (N, Acc,  F) -> F(N-1, N * Acc, F)
		end,
	    G(X, 1, G)
    end.

