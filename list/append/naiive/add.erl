-module(add).
-export([add/2]).
-include_lib("eunit/include/eunit.hrl").

add_test_() ->
    [
     {"Given [a,b] and [c]; Then must return [a,b,c]", ?_assertEqual([a,b,c], add([a,b],[c]))},
     
     {"Expect Error when passing a non-list argument in first parameter", ?_assertError(badarg, add(n,[a]))}
    ].

add([], L2) ->
    L2;
add(L1, []) ->
    L1;
add(L1, L2) ->
    L1 ++ L2. % or L1 ++ [L2]. % but again it will add a list to the first as nested. 

