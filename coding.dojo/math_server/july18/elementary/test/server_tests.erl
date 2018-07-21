-module(server_tests).
-include_lib("eunit/include/eunit.hrl").
-import(server, [start/0, stop/1, sum_areas/2, async_sum_areas/2]).

start_test() ->
    {ok, Pid} = start(),

    ?assert(is_process_alive(Pid)),

    {ok, noreply} = stop(Pid).

stop_test() ->
    {ok, Pid} = start(),
    ?assert(is_process_alive(Pid)),

    {ok, noreply} = stop(Pid),
    receive after 1 -> ok end,

    ?assertNot(is_process_alive(Pid)).

sum_areas_test() ->    
    {ok, Pid} = start(),
    Shapes = [{circle, 3}, {rectangle, 3, 4}],

    {reply, {ok, Sum}} = sum_areas(Shapes, Pid),

    ?assertEqual(40.27433388230814, Sum),

    {ok, noreply} = stop(Pid).

async_sum_areas_test() ->
    {ok, Pid} = start(),
    Shapes = [{circle, 3}, {rectangle, 3, 4}],

    {ok, noreply} = async_sum_areas(Shapes, Pid),

    receive
	{reply, {ok, Sum}} ->
	    ?assertEqual(40.27433388230814, Sum)
    after 3000 ->
	    exit(timeout)
    end,

    {ok, noreply} = stop(Pid).

sum_areas_unknown_shapes_test() ->
    {ok, Pid} = start(),
    Shapes = [{ellipse, 3, 4}],

    M = sum_areas(Shapes, Pid),

    ?assertMatch({reply, {error, {function_clause, _Detail}}}, M),

    {ok, noreply} = stop(Pid).

async_sum_areas_unknown_shapes_test() ->
    {ok, Pid} = start(),
    Shapes = [{ellipse, 3 ,4}],

    {ok, noreply} = async_sum_areas(Shapes, Pid),

    receive
	M ->
	    ?assertMatch({reply, {error, {function_clause, _Detail}}}, M)
    end,

    {ok, noreply} = stop(Pid).
