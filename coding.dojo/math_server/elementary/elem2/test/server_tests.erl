-module(server_tests).
-include_lib("eunit/include/eunit.hrl").
-import(server, [start/0, sum_areas/2, stop/1]).

start_test() ->
    ServerPid = start(),

    ?assertMatch(Pid when is_pid(Pid), ServerPid).

sum_areas_test() ->
    Shapes = [{circle, 3}, {rectangle, 4, 6}],
    ServerPid = start(),

    Result = sum_areas(Shapes, ServerPid),

    ?assertEqual({ok, 52.27433388230814}, Result).

stop_test() ->
    ServerPid = start(),

    stop(ServerPid),

    receive after 50 -> ok end,
    ?assertNot(is_process_alive(ServerPid)).

sum_unknown_areas_test() ->
    ServerPid = start(),
    Shapes = [{circle, 3, 3}],

    Result = sum_areas(Shapes, ServerPid),

    ?assertMatch({error, {function_clause, _Detail}}, Result).
