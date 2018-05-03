-module(server_tests).
-include_lib("eunit/include/eunit.hrl").
-import(server, [start/0, cast/2]).

start_ok_test() ->
    MathServerPid = start(),
    
    ?assertMatch(Pid when is_pid(Pid), MathServerPid),
    ?assertEqual(true, is_process_alive(MathServerPid)).

asynchronous_cast_respond_ok_test() ->
    Shapes = [{circle, 3}, {rectangle, 3, 4}],
    MathServerPid = start(),
    
    Return = cast(MathServerPid, {print, Shapes}),
   
    ?assertEqual(ok, Return).
