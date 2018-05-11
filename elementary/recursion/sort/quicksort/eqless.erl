-module(eqless).
-export([eqless/2]).

eqless(_,[]) ->
    [];
eqless(Pivot, [H|T]) ->
    if  H =< Pivot ->
	    [H|eqless(Pivot,T)];
        true -> eqless(Pivot,T)
    end.
