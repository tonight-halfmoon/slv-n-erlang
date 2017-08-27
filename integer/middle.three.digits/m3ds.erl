-module(m3ds).
-include_lib("eunit/include/eunit.hrl").
-export([run/1]).

digitise_test()->
    ?assertEqual([1,2,3], digitize(123)).
only_three_digits_test() ->
    ?assertEqual([1, 2, 3], run(123)).
only_three_digits_neg_test() ->
    ?assertEqual([2, 1, 3], run(-213)).
lessthn3ds_test() ->
    ?assertEqual(na, run(12)).

moreth3ds_test() ->
    ?assertEqual([1,3,4], run(5213415)).

moreth3dsNotSym_test() ->
    ?assertEqual(na, run(521345)).
moreth3dsEven8_test()->
    ?assertEqual(na, run(33456789)).
moreth3dsEven9_test()->
    ?assertEqual([4,5,6], run(133456789)).
moreth3dsEven10_test()->
    ?assertEqual(na, run(2133456789)).

%% API
run(Int) when abs(Int) >= 0, 
		  abs(Int) < 100 ->
    na;
run(Int) ->
    L = digitize(Int),
    run(L, length(L)).

run([D1,D2,D3], _) ->
    [D1,D2,D3];
run(_ , Len) when Len rem 2 =:= 0 ->
    na;
run([_ |Tail], Len) ->
    [_FT| TL] = lists:reverse(Tail),
    run(lists:reverse(TL), length(TL));
run(Ds, _) ->
    Ds.

digitize(Int) -> 
   lists:map(fun(X) -> list_to_integer(X) end, digitize(abs(Int), [])).

digitize(Int, Ds) when Int >= 0, Int < 10 ->
    [integer_to_list(Int)|Ds];
digitize(Int, Ds) ->
    digitize(Int div 10, [integer_to_list(Int rem 10) | Ds]).
