-module(server_tests).
-include_lib("eunit/include/eunit.hrl").
-import(server, [start/0, sum_areas/2, stop/1]).

start_test() ->
    {ok, ServerPid} = start(),

    ?assertMatch(Pid when is_pid(Pid), ServerPid),

    {ok, stopped, _state} = stop(ServerPid).

sum_areas_test() ->
    {ok, Pid} = start(),
    Shapes = [{circle, 3}, {rectangle, 4, 6}],

    Result = sum_areas(Shapes, Pid),

    ?assertEqual({ok, 52.27433388230814}, Result),

    {ok, stopped, _state} = stop(Pid).

stop_test() ->
    {ok, Pid} = start(),

    {ok, stopped, {}} = stop(Pid),

    ?assertNot(is_process_alive(Pid)),

    {error, already_stopped} = stop(Pid).

sum_areas_unknown_shapes_test() ->
    {ok, Pid} = start(),
    Shapes = [{ellipse, 3, 3}],

    Result = sum_areas(Shapes, Pid),

    ?assertMatch({error, {function_clause, _Detail}}, Result),

    {ok, stopped, _Error} = stop(Pid).

server_send_last_shapes_areas_sum_to_client_when_stop_test() ->
    Shapes = [{circle, 0.873}, {square, 0.293}],
    ExpectedState = {{ok, 2.4801478674877355}, Shapes},
    {ok, Pid} = start(), 
    _Sum = sum_areas(Shapes, Pid),

    {ok, stopped, State} = stop(Pid),

    ?assertEqual(ExpectedState, State).
