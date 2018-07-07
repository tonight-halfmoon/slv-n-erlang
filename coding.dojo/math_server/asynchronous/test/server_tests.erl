-module(server_tests).
-include_lib("eunit/include/eunit.hrl").
-import(server, [start/0, stop/1,
		 sum_areas/2,
		 async_sum_areas/2]).

start_test() ->
    {ok, Pid} = start(),

    ?assert(is_process_alive(Pid)),

    {ok, stopped} = stop(Pid).

sum_areas_test() ->
    Shapes = [{circle, 3}, {rectangle, 3, 4}],
    {ok, Pid} = start(),

    {ok, Sum} = sum_areas(Shapes, Pid),

    ?assertEqual(40.27433388230814, Sum),

    {ok, stopped} = stop(Pid).

stop_test() ->
    {ok, Pid} = start(),
    ?assert(is_process_alive(Pid)),

    {ok, stopped} = stop(Pid),
    receive after 1 -> ok end,

    ?assertNot(is_process_alive(Pid)).

asynchronous_sum_areas_test() ->
    Shapes = [{circle, 3}, {rectangle, 3, 4}],
    {ok, Pid} = start(),

    {ok, noreply} = async_sum_areas(Shapes, Pid),

    receive
	{ok, Sum} ->
	    ?assertEqual(40.27433388230814, Sum)
    end,

    {ok, stopped} = stop(Pid).

sum_areas_unknown_shapes_test() ->
    Shapes = [{ellipse, 3, 4}],
    {ok, Pid} = start(),
    
    M = sum_areas(Shapes, Pid),

    ?assertMatch({error, {function_clause, _Detail}}, M),

    {ok, stopped} = stop(Pid).
