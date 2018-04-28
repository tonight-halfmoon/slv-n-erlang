-module(math_server_tests).
-include_lib("eunit/include/eunit.hrl").

-import(math_server, [start/0, stop/0, call/1]).

-include("math_server.hrl").

start_test() ->
    start(),
    ?assert(is_process_alive(whereis(?MathServer))),

    aftereach().

already_started_test() ->
    start(),
    ?assert(is_process_alive(whereis(?MathServer))),

    Result = case catch start() of
		 M ->
		     M
	     end,
    
    ?assertEqual(already_started, Result),
    
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

call_server_respond_with_areas_calculated_test() ->
    start(),
    ?assert(is_process_alive(whereis(?MathServer))),
    Shapes = [{circle, 3}],

    Areas = call(Shapes),
    aftereach(),

    ?assertEqual(28.274333882308138, Areas).

call_notify_user_when_something_went_wrong_test() ->
    start(),
    ?assert(is_process_alive(whereis(?MathServer))),
    Shapes = [{ellipse, 3, 6}],
    
    Reply = call(Shapes),
    aftereach(),
    
    ?assertMatch({error,
		  {'EXIT',
		   {function_clause, _Detail}}}, Reply).

call_when_server_has_shutdown_test() ->
    start(),
    ?assert(is_process_alive(whereis(?MathServer))),
    Shapes = [{circle, 3}],
    stop(),
    
    Result = case catch call(Shapes) of
		 M ->
		     M 
	     end,
   
    ?assertEqual({'EXIT', timeout}, Result).

aftereach() ->
    stop().
