-module(fizzbuzz_tests).

-include_lib("eunit/include/eunit.hrl").

-import(fizzbuzz, [fb/1]).

fb_return_number_test_() ->
    {
      "fb when number is not multiple of 3 or 5, then return number",
      ?_assertEqual(1, fb(1))
    }.
