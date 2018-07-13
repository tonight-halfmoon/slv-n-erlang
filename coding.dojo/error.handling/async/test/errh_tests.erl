-module(errh_tests).
-include_lib("eunit/include/eunit.hrl").
-import(errh, [start/1, stop/1, increment/1]).

proc_b_terminates_when_proc_a_crash_test() ->
    {ok, Pida} = start(0),
    {ok, Pidb} = start({0, Pida}),

    {ok, noreply} = increment({tatata, Pida}),
    receive after 1 -> ok end,

    ?assertNot(is_process_alive(Pida)),
    ?assertNot(is_process_alive(Pidb)).

increment_test() ->
    {ok, Pida} = start(0),

    {ok, noreply} = increment({4, Pida}),

    M = receive
	    {reply, Pida, Output} ->
		Output
	after 1 ->
		ok
	end,

    ?assertEqual(5, M).
