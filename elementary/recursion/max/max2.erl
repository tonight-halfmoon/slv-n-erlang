-module(max2).
-export([max/1]).
-include_lib("eunit/include/eunit.hrl").

max_test()->
    Expected_value = 4342,
    L = [1,2,4,5,8,23,554,3,89,2133,4342,2],
    ?assertEqual(Expected_value, max(L)).

max([])->
    [];
max([H|T]) ->
   max(T, H).
max([H|T], Max) when H > Max ->
    max(T, H);
max([], Max) ->
    Max;
max([_|T], Max) ->
    max(T, Max).


    
