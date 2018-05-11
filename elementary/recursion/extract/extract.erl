-module(extract).
-export([nth/2]).
-include_lib("eunit/include/eunit.hrl").

nth_not_found_test()->
    Expected_value = not_found,
    L = [y,f,g,5,h,e,a],
    Nth = 8,
    ?assertEqual(Expected_value, nth(Nth,L)).
    
nth_test()->
    Expected_value = g,
    L = [y,f,g,5,h,e,a],
    Nth = 3,
    ?assertEqual(Expected_value, nth(Nth,L)).

nth(Nth, L)->
    nth(1, Nth, L).
nth(Max, Max, [H|_])->
    H;
nth(C, Nth, [_|T]) ->
    nth(C+1, Nth, T);
nth(_,_,[]) ->
    not_found.

