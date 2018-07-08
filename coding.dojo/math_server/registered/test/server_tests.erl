-module(server_tests).
-include_lib("eunit/include/eunit.hrl").

-import(server, [start/0, stop/0, sum_areas/1]).

-include("server.hrl").

start_test() ->
    {ok, _Pid} = start(),

    ?assert(is_process_alive(whereis(?math_server))),

    {ok, stopped} = stop().

already_started_test() ->
    {ok, _Pid} = start(),

    Reply = case catch start() of
		M ->
		    M
	    end,

    ?assertEqual({error, already_started}, Reply),

    {ok, stopped} = stop().

stop_test() ->
    {ok, Pid} = start(),

    {ok, stopped} = stop(),

    receive after 1 ->
		    ok
	    end,

    ?assertEqual(undefined, whereis(?math_server)),
    ?assertNot(is_process_alive(Pid)),

    {error, already_stopped} = stop().

sum_areas_test() ->
    Shapes = [{circle, 3}],
    {ok, _Pid} = start(),

    Reply = sum_areas(Shapes),

    {ok, stopped} = stop(),

    ?assertEqual({ok, 28.274333882308138}, Reply).

sum_areas_unknown_shapes_test() ->
    Shapes = [{ellipse, 3, 6}],
    {ok, _Pid} = start(),

    Reply = sum_areas(Shapes),

    {ok, stopped} = stop(),

    ?assertMatch({error,
		  {function_clause, _Detail}}, Reply).

timeout_test() ->
    Shapes = [{circle, 3}],
    {ok, _Pid} = start(),

    stop(),

    Reply = case catch sum_areas(Shapes) of
		M ->
		    M
	    end,

    ?assertEqual({'EXIT', timeout}, Reply).
