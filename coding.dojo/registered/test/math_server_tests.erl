-module(math_server_tests).
-include_lib("eunit/include/eunit.hrl").

-import(math_server, [start/0, stop/0, call/1]).

-include("math_server.hrl").

start_test() ->
    start(),

    ?assert(is_process_alive(whereis(?MathServer))),

    aftereach().

stop_test() ->
    start(),
    ?assert(is_process_alive(whereis(?MathServer))),
    MathServerPid = whereis(?MathServer),

    stop(),
    receive after 50 -> ok end,

    ?assertEqual(undefined, whereis(?MathServer)),
    ?assertNot(is_process_alive(MathServerPid)),
    aftereach().

call_shapes_request_math_server_respond_areas_test() ->
    start(),
    Shapes = [{circle, 3}],

    Areas = call(Shapes),

    ?assertEqual(28.274333882308138, Areas),
    aftereach().

aftereach() ->
    stop().

