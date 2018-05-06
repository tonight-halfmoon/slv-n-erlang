-module(server_tests).
-include_lib("eunit/include/eunit.hrl").
-import(server, [start/0, sum_areas/2]).

start_test() ->
    {ok, ServerPid} = start(),

    ?assertMatch(Pid when is_pid(Pid), ServerPid),
    ?assertEqual(true, is_process_alive(ServerPid)).

asynchronous_sum_areas_test() ->
    Shapes = [{circle, 3}, {rectangle, 3, 4}],
    {ok, Pid} = start(),

    sum_areas(Shapes, Pid),

    receive
	M ->
	    ?assertEqual({ok, 40.27433388230814}, M)
    end.
