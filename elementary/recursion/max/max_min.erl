-module(max_min).
-export([max_min/1]).

max_min([Head|Res])->
    max_min_tail(Res, Head, Head).

max_min_tail([Head|Res], Max, Min) ->
  if
      Head > Max ->
	  Max_Val = Head;
      true ->
	  Max_Val = Max
    end,
  if 
    Head < Min -> 
	  Min_Val = Head;
      true ->
	  Min_Val = Min
  end,
    max_min_tail(Res, Max_Val, Min_Val);
			      
max_min_tail([], Max,Min) ->
    {Max,Min}.
