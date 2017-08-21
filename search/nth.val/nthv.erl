-module(nthv).
-include_lib("eunit/include/eunit.hrl").
-export([nth/2]).

nth_must_return_nothing_when_not_in_range_test()->
    ?assertEqual(nothing, nth(8, [y,f,g,5,h,e,a])).

nth_must_return_the_nth_value_provided_when_in_range_test() ->    
    ?assertEqual(g, nth(1, [g,a])).

val_at_test()->
    ?assertEqual(g, nth(3, [y,f,g,5,h,e,a])).

nth(Nth, L) ->
    nth(1, Nth, L).

nth(_,_,[]) ->
    nothing;
nth(Max, Max, [H|_])->
    H;
nth(C, Nth, [_|T]) ->
    nth(C+1, Nth, T).

