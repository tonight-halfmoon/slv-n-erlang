-module(server_tests).
-include_lib("eunit/include/eunit.hrl").
-import(server, [start/0, sum_areas/2, stop/1]).

start_test() ->
    {ok, ServerPid} = start(),

    ?assert(is_process_alive(ServerPid)),

    {ok, stopped} = stop(ServerPid).

sum_areas_test() ->
    {ok, Pid} = start(),
    Shapes = [{circle, 3}, {rectangle, 4, 6}],

    Reply = sum_areas(Shapes, Pid),

    ?assertEqual({ok, 52.27433388230814}, Reply),

    {ok, stopped} = stop(Pid).

stop_test() ->
    {ok, Pid} = start(),

    {ok, stopped} = stop(Pid),
    
    receive after 1 ->
		    ok
	    end,

    ?assertNot(is_process_alive(Pid)),

    {ok, stopped} = stop(Pid).

sum_areas_unknown_shapes_test() ->
    {ok, Pid} = start(),
    Shapes = [{ellipse, 3, 3}],

    Reply = sum_areas(Shapes, Pid),

    ?assertMatch({error, {function_clause, _Detail}}, Reply),

    {ok, stopped} = stop(Pid).
