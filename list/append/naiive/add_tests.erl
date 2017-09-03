-module(add_tests).
-include_lib("eunit/include/eunit.hrl").

extra_tests_4_add_module_test_() ->
    [
     {"Given two empty lists; then must return an empty list", ?_assertEqual([], add:add([],[]))},
     {"Given [a] and [a]; Then must return [a,a]", ?_assertEqual([a,a], add:add([a],[a]))},
      [
       {"Expect error 'undef' when passing any()", ?_assertError(undef, add:add(eunit:any(),eunit:any()))},
       {"Expect to return L1 when passed with an empty L2", ?_assertEqual([1,2,3,a], add:add([1,2,3,a], []))},
       {"Expect to return L2 when passed with an empty L1", ?_assertEqual([1,2,3,a], add:add([], [1,2,3,a]))},
       {"Expect to retain the original order", ?_assertEqual([1,2,3,a], add:add([1,2],[3,a]))},
       {"Expect to retain the original order", ?_assertEqual([2,1,a,3], add:add([2,1],[a,3]))}
     ]
    ].
