-module(server_tests).
-include_lib("eunit/include/eunit.hrl").

-import(server, [start/0, stop/0, sum_areas/2,
		 connect_client/1]).

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
    {ok, _Pid} = start(),

    ServerPid = whereis(?math_server),

    stop(),
    
    receive after 1 ->
		    ok
	    end,

    ?assertEqual(undefined, whereis(?math_server)),
    ?assertNot(is_process_alive(ServerPid)),

    aftereach().

sum_areas_test() ->
    {ok, _Pid} = start(),
    Shapes = [{circle, 0.3}],
    Client = spawn(fun() -> receive {reply, {sum_areas, ok, Areas}} ->
				    ?assertEqual(0.2827433388230814, Areas) after 4 -> exit(timeout) end end),
    {ok, client_connected, Client} = connect_client(Client),

    sum_areas(Shapes, Client),

    receive after 4 ->
		    ok
	    end,

    exit(Client, unit_testing),
    aftereach().

sum_areas_unknown_shapes_test() ->
    {ok, _Pid} = start(),
    Shapes = [{ellipse, 3, 6}],
    Client = spawn(fun() ->  receive Reply ->
				      ?assertMatch({reply, {sum_areas, {error, {function_clause, _Detail}}}}, Reply)
			      after 4 -> exit(timeout) end end),

    sum_areas(Shapes, Client),

    receive after 4 -> ok end,
    exit(Client, unit_testing),
    aftereach().

timeout_test() ->
    {ok, _Pid} = start(),
    Shapes = [{circle, 3}],
    stop(),
    Client = spawn(fun() -> receive Reply -> ?assertEqual({'EXIT', timeout}, Reply) after 4 -> exit(timeout) end end),

    sum_areas(Shapes, Client),

    exit(Client, unit_testing),
    aftereach().

when_client_disconnected_server_shutdown_test() ->
    {ok, ServerPid} = start(),
    {ok, client_connected, Client} = connect_client(spawn(fun() -> receive _ -> ok end end)),

    receive after 5 -> ok end,

    exit(Client, unit_testing),

    receive after 4 ->
		    ok
	    end,

    ?assertNot(is_process_alive(ServerPid)),
    ?assertEqual(undefined, whereis(?math_server)),

    aftereach().

aftereach() ->
    stop().
