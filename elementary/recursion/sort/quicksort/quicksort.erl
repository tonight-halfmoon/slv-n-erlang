-module(quicksort).
%-export([qs/1]).
-export([quicksort/1]).

%qs([]) -> [];
%qs([Pivot|Rest])->
%    {Eqless, Gr} = partition(Pivot, Rest, [], []),
%    qs(Eqless) ++ [Pivot] ++ qs(Gr).

%partition(_,[],Eqless,Gr) -> {Eqless, Gr};
%partition(Pivot, [H|T], Eqless, Gr) -> 
%    if H =< Pivot ->
%	    partition(Pivot,T, [H|Eqless], Gr);
%       H > Pivot ->
%	    partition(Pivot,T, Eqless,[H|Gr])
%    end.


%quicksort([]) ->
%    [];
%quicksort([Pivot|Rest]) ->
%    {Eqless, Gr} = partition(Pivot, Rest, [], []),
%    qs(Eqless) ++ [Pivot] ++ qs(Gr).

%partition(_,[], Eqless, Gr) ->
%    {Eqless, Gr};
%partition(Pivot, [H|T], Eqless, Gr) ->
%    if H =< Pivot ->
%	    partition(Pivot, T, [H|Eqless], Gr);
%       H > Pivot ->
%	    partition(Pivot, T, Eqless, [H|Gr])
%end.

quicksort([])->
    [];
quicksort([Pivot|Rest]) ->
    {Qls, Lrg} = partition(Pivot, Rest, [], []),
    quicksort(Qls) ++ [Pivot] ++ quicksort(Lrg).

partition(_, [], Qls, Lrg) ->
    {Qls, Lrg};
partition(Pivot, [H|T], Qls, Lrg) ->
    if H =< Pivot ->
	    partition(Pivot, T, [H|Qls], Lrg);
       H > Pivot ->
	    partition(Pivot, T, Qls, [H|Lrg])
end.

