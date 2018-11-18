-module(fizzbuzz_tests).

-include_lib("eunit/include/eunit.hrl").

-import(fizzbuzz, [fb/1]).

fb_return_number_test_() ->
    {
      "fb when number is not multiple of 3 or 5, then return number",
      ?_assertEqual(1, fb(1))
    }.

fb_return_fizz_test_() ->
   {
     "fb when number is multiple of 3, then return 'Fizz'",
     ?_assertEqual('Fizz', fb(3))
   }.

fb_return_buzz_test_() ->
    {
      "fb when number is multiple of 5, then return 'Buzz'",
      ?_assertEqual('Buzz', fb(5))
    }.

fb_return_FizzBuzz_test_() ->
    {
      "fb when number is multiple of 3 and 5, then return 'FizzBuzz'",
      ?_assertEqual('FizzBuzz', fb(15))
    }.
