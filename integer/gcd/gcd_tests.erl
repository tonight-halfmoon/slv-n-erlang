-module(gcd_tests).
-include_lib("eunit/include/eunit.hrl").

%%  eunit:test([{inparallel, gcd}, gcd_tests], [{report, {eunit_surefire, [{dir, "."}]}}, verbose]).	
gcd_1_1_test_() ->
    ?_assertEqual(1, gcd:gcd(1,1)).

gcd_1_0_test_() ->
    ?_assertEqual(1, gcd:gcd(1,0)).

gcd_0_1_test_() ->
    ?_assertEqual(1, gcd:gcd(0,1)).

gcd_0_x_test_() ->
    ?_assertEqual(9, gcd:gcd(0,9)).

gcd_x_0_test_() ->
    ?_assertEqual(9, gcd:gcd(9,0)).

gcd_neg2147483648_neg2147483648_test_() ->
    ?_assertEqual(2147483648, gcd:gcd(-2147483648,-2147483648)).

gcd_neg2147483649_neg2147483649_test_() ->
    ?_assertEqual(2147483649, gcd:gcd(-2147483649,-2147483649)).

gcd_456_9147483649232_test_() ->
    ?_assertEqual(8, gcd:gcd(456,9147483649232)).

gcd_2147483647_2147483647_test_() -> 
    ?_assertEqual(2147483647, gcd:gcd(2147483647,2147483647)).

gcd_214748_21474836_test_() ->
    ?_assertEqual(4, gcd:gcd(214748,21474836)).

gcd_neg24800_neg8_test_() ->
    ?_assertEqual(8, gcd:gcd(-24800,-8)).

gcd_456_9197989699992_test_() ->
    ?_assertEqual(24, gcd:gcd(456, 9197989699992)).

gcd_24_568312_test_() ->
    {"'gcd(24, 568312)' must yield in '8'", ?_assertEqual(8, gcd:gcd(24, 568312))}.

gcd_9_99999999999_test_() ->
    {"'gcd(9,99999999999) must yield in '9'", ?_assertEqual(9, gcd:gcd(9,99999999999))}.

gcd_9_lots_of9s_test_() ->
    {"'gcd(9,99999999999) must yield in '9'", ?_assertEqual(9, gcd:gcd(9,9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999))}.

%%gcd_2_2pow64_test_() ->
%%    {"'gcd(2, 1.8446744073709552e19)' must yield in '1'", ?_assertEqual(1, gcd:gcd(2, 1.8446744073709552e19))}.
