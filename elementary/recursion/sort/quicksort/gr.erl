-module(gr).
-export([gr/2]).


gr(Pivot,T)->
    tail_gr(Pivot,T,[]).

tail_gr(_,[],List) ->
    List;
tail_gr(Pivot,[H|T], List) -> 
    if  H > Pivot ->
	    tail_gr(Pivot, T, [H|List]);
	true -> tail_gr(Pivot, T,List)
    end.
