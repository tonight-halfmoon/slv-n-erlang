-module(max_min_2).
-export([max_min/1]).

max_min([H|Res]) -> 
   % max_min(Res, H, H).
    {max_m(Res, H), min_m(Res, H)}.

max_min([], Mx,Mn) ->
    {Mx,Mn};

max_min([H|Res], Mx, Mn) ->
    if H > Mx ->
	    Max = H
		;
       H =< Mx ->
	    Max= Mx
    end,
    if H < Mn ->
	    Min = H
		;
        H >= Mn ->
	    Min= Mn
    end,
    max_min(Res, Max, Min).

max_m([H|Res], Max) 
  when H > Max ->
    max_m(Res, H);
max_m([], Max) ->
    Max;
max_m([_|Res], Mx) ->
    max_m(Res,Mx).


min_m([H|Res], Mn)
  when H < Mn ->
    min_m(Res, H);
min_m([], Mn) ->
    Mn;
min_m([_|Res], Mn) ->
    min_m(Res,Mn).


    
