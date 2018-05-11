-module(reverse2).
-export([rev/1]).
-include_lib("eunit/include/eunit.hrl").

run_test() ->
    ?assertEqual(rev([1,2,3,4]),[4,3,2,1]),
    ?assertEqual(rev([a,b,c]),[c,b,a]),
    ?assertEqual(rev([]),[]),
    ?assertEqual(rev([a]),[a]),
    ?assertEqual(rev(a),ok).

rev([]) ->
    [];
rev(Input) when not(is_list(Input)) ->
    io:format("Expect Input of type List~n");
rev([H|T])->
    rev(T, [H]).
rev([], Out) ->
    Out;
rev([H|T], Out) ->
    rev(T, [H| Out]).
