-module(server_tests).
-include_lib("eunit/include/eunit.hrl").

-import(server, [start/0, stop/0, sum_areas/1]).

-include("server.hrl").

start_test() ->
    {ok, _Pid} = start(),

    ?assert(is_process_alive(whereis(?math_server))),

    aftereach().

already_started_test() ->
    {ok, _Pid} = start(),

    Result = case catch start() of
		 M ->
		     M
	     end,

    ?assertEqual({error, already_started}, Result),

    aftereach().

stop_test() ->
    {ok, Pid} = start(),

    stop(),

    receive after 1 -> ok end,

    ?assertEqual(undefined, whereis(?math_server)),
    ?assertNot(is_process_alive(Pid)),

    aftereach().

sum_areas_test() ->
    {ok, _Pid} = start(),
    Shapes = [{circle, 3}],

    {ok, Sum} = sum_areas(Shapes),

    aftereach(),

    ?assertEqual(28.274333882308138, Sum).

sum_areas_unknown_shapes_test() ->
    {ok, _Pid} = start(),

    Shapes = [{ellipse, 3, 6}],

    Reply = sum_areas(Shapes),

    aftereach(),

    ?assertMatch({error,
		  {'EXIT',
		   {function_clause, _Detail}}}, Reply).

timeout_test() ->
    {ok, _Pid} = start(),
    Shapes = [{circle, 3}],
    stop(),

    Result = case catch sum_areas(Shapes) of
		 M ->
		     M
	     end,

    ?assertEqual({'EXIT', timeout}, Result).

aftereach() ->
    stop().
