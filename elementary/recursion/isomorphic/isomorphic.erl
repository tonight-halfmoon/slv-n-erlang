-module(isomorphic).
-export([double/1]).
-include_lib("eunit/include/eunit.hrl").

double_test()->
    Expected_value = [2,4,8,[12,14,16, [20,16,[100]]]],
    L = [1,2,4, [6,7,8, [10,8,[50]]]],
    ?assertEqual(Expected_value, double(L)).

double([])->[];
double([H|T]) when is_integer(H)->
    [H *2 | double(T)];
double([H|T]) when is_list(H) ->
    [double(H)|double(T)].
