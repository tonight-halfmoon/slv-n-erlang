% Khobar, SA ; Dec 28th, 2015. Costa cafe Shore.
-module(max_tail).
-export([max/1]).

max([H|T]) -> tail_max([H|T], H).
    %tail_max(T, H).
tail_max([], Max)->
    Max;

tail_max([H|T], Max) ->
    if H < Max ->
	    tail_max(T, Max);
        H >= Max->
	    tail_max(T, H)
end.
