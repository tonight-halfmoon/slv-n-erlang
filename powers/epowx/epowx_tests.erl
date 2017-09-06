-module(epowx_tests).
-include_lib("eunit/include/eunit.hrl").
-import(epowx, [epowx/1]).

extra_epowx_test_() ->
    [
     {"Expect 'e'^5.0000 to evaluate as '143.6895'", ?_assertEqual(143.6895, epowx(5.0000))},
     {"Expect 'e^0.5000' to evaluate as '1.6487'", ?_assertEqual(1.6487, epowx(0.5000))},
     {"Expect 'e^-0.5000' to evaluate as '0.6065'", ?_assertEqual(0.6065, epowx(-0.5000))}
    ].
