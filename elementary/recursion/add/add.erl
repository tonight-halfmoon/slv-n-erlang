-module(add).
-export([add/2]).
-include_lib("eunit/include/eunit.hrl").

add_test_() ->
    [
     {"Given [a,b] and [c]; Then must return [a,b,c]", ?_assertEqual([a,b,c], add([a,b],[c]))},
     [ 
       {"Expect error 'undef' when passing any()", ?_assertError(undef, add(eunit:any(),eunit:any()))},
       {"Expect to return L1 when passed with an empty L2", ?_assertEqual([1,2,3,a], add([1,2,3,a], []))},
       {"Expect to return L2 when passed with an empty L1", ?_assertEqual([1,2,3,a], add([], [1,2,3,a]))},
       {"Expect to retain the original order", ?_assertEqual([1,2,3,a], add([1,2],[3,a]))},
       {"Expect to retain the original order", ?_assertEqual([2,1,a,3], add([2,1],[a,3]))}
     ],
     {"Expect Error when passing a non-list argument in first parameter", ?_assertError(badarg, add(n,[a]))}
    ].

add([], Elm) ->
    Elm;
add(Set, []) ->
    Set;
add(Set, Elm) ->
    Set ++ Elm. % or Set ++ [Elm]. % but again it will add a list to the first as nested. 

