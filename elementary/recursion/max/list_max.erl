% Khobar, SA ; Dec 28th, 2015. Costa cafe Shore.
-module(list_max).
-export([list_max/1]).

list_max([H|T]) -> list_max(T, H).

list_max([], Max)->
    Max;

list_max([H|T], Max) when H > Max -> 
			list_max(T, H);
list_max([_|T], Max) ->
    list_max(T, Max).

    %if H < Max ->
	%    max(T, Max);
        %H >= Max->
	 %   max(T, H)
%end.
