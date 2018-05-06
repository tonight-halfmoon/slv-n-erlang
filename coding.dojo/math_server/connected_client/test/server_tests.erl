-module(server_tests).
-include_lib("eunit/include/eunit.hrl").

-import(server, [start/0, stop/0, sum_areas/1,
		 connect_client/1]).

-include("server.hrl").

start_test() ->
    {ok, _Pid} = start(),
    ?assert(is_process_alive(whereis(?math_server))),

    aftereach().

already_started_test() ->
    {ok, _Pid} = start(),
    ?assert(is_process_alive(whereis(?math_server))),

    Result = case catch start() of
		 M ->
		     M
	     end,

    ?assertEqual({error, already_started}, Result),

    aftereach().

stop_test() ->
    {ok, _Pid} = start(),
    ?assert(is_process_alive(whereis(?math_server))),
    ServerPid = whereis(?math_server),

    stop(),

    receive after 1 -> ok end,

    ?assertEqual(undefined, whereis(?math_server)),
    ?assertNot(is_process_alive(ServerPid)),

    aftereach().

sum_areas_test() ->
    {ok, _Pid} = start(),
    ?assert(is_process_alive(whereis(?math_server))),
    Shapes = [{circle, 3}],

    {ok, Sum} = sum_areas(Shapes),
    aftereach(),

    ?assertEqual(28.274333882308138, Sum).

sum_areas_unknown_shapes_test() ->
    {ok, _Pid} = start(),
    ?assert(is_process_alive(whereis(?math_server))),
    Shapes = [{ellipse, 3, 6}],

    Reply = sum_areas(Shapes),

    aftereach(),

    ?assertMatch({error,
		  {'EXIT',
		   {function_clause, _Detail}}}, Reply).

timeout_test() ->
    {ok, _Pid} = start(),
    ?assert(is_process_alive(whereis(?math_server))),
    Shapes = [{circle, 3}],
    stop(),

    Result = case catch sum_areas(Shapes) of
		 M ->
		     M
	     end,

    ?assertEqual({'EXIT', timeout}, Result).

when_client_disconnected_server_shutdown_test() ->
    Client = spawn(fun() -> process_flag(trap_exit, true), receive {'EXIT', _F, W} -> exit(W); M -> M end end),

    {ok, ServerPid} = start(),
    {ok, client_connected, Client} = connect_client(Client),
    Shapes = [{circle, 4}],
    sum_areas({Client, Shapes}),

    exit(Client, unit_testing),

    receive after 4 -> ok end,

    ?assertNot(is_process_alive(ServerPid)),
    ?assertEqual(undefined, whereis(?math_server)).

aftereach() ->
    stop().
