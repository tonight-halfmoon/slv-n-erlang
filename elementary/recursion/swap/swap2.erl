%%%%% 7th Feb 2016; Jeddah, SA
-module(swap2).
-export([swp/1]).
-include_lib("eunit/include/eunit.hrl").

run_test()->
    ?assertEqual(swp([a,b,c,d]), [b,a,d,c]),
    ?assertEqual(swp([a,b,c,d,f]), [b,a,d,c,f]),
    ?assertEqual(swp([]), []),
    ?assertEqual(swp([a]), [a]).

swp([]) ->
    [];
swp(Input) ->
    swp(Input, []).
swp([], Out) ->
    Out;
swp([A], Out)->
    swp([], Out ++ [A]);
swp([H,H2|T], Out) ->
    swp(T, Out ++ [H2,H]).
