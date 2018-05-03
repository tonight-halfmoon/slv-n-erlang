-module(server_tests).
-include_lib("eunit/include/eunit.hrl").

-import(server, [start/0, stop/0, call/1]).

-include("server.hrl").

start_test() ->
    start(),
    ?assert(is_process_alive(whereis(?math_server))),

    aftereach().

already_started_test() ->
    start(),
    ?assert(is_process_alive(whereis(?math_server))),

    Result = case catch start() of
		 M ->
		     M
	     end,

    ?assertEqual({error, already_started}, Result),

    aftereach().

stop_test() ->
    start(),
    ?assert(is_process_alive(whereis(?math_server))),
    MathServerPid = whereis(?math_server),

    stop(),
    receive after 50 -> ok end,

    ?assertEqual(undefined, whereis(?math_server)),
    ?assertNot(is_process_alive(MathServerPid)),
    aftereach().

call_server_respond_with_areas_calculated_test() ->
    start(),
    ?assert(is_process_alive(whereis(?math_server))),
    Shapes = [{circle, 3}],

    Areas = call(Shapes),
    aftereach(),

    ?assertEqual(28.274333882308138, Areas).

call_notify_user_when_something_went_wrong_test() ->
    start(),
    ?assert(is_process_alive(whereis(?math_server))),
    Shapes = [{ellipse, 3, 6}],

    Reply = call(Shapes),
    aftereach(),

    ?assertMatch({error,
		  {'EXIT',
		   {function_clause, _Detail}}}, Reply).

call_when_server_has_shutdown_test() ->
    start(),
    ?assert(is_process_alive(whereis(?math_server))),
    Shapes = [{circle, 3}],
    stop(),

    Result = case catch call(Shapes) of
		 M ->
		     M
	     end,

    ?assertEqual({'EXIT', timeout}, Result).

aftereach() ->
    stop().
