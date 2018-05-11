-module(add_tests).
-include_lib("eunit/include/eunit.hrl").

extra_tests_4_add_module_test_() ->
    [
     {"Given two empty lists; then must return an empty list", ?_assertEqual([], add:add([],[]))},
     {"Given [a] and [a]; Then must return [a,a]", ?_assertEqual([a,a], add:add([a],[a]))}
    ].
