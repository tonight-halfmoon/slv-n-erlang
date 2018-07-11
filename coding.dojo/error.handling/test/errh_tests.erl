-module(errh_tests).
-include_lib("eunit/include/eunit.hrl").
-import(errh, [start/1, stop/1]).

process_a_terminate_when_process_b_terminate_test() ->
    {ok, Pida} = start(0),
    {ok, Pidb} = start({0, Pida}),

    {ok, noreply} = stop(Pida),
    receive after 1 -> ok end,

    ?assertNot(is_process_alive(Pida)),
    ?assertNot(is_process_alive(Pidb)).
