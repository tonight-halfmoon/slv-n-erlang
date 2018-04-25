-module(math_server_tests).
-include_lib("eunit/include/eunit.hrl").
-import(math_server, [start/0]).

server_start_ok_test() ->
    MathServer = start(),
    
    ?assertMatch(Pid when is_pid(Pid), MathServer).

server_response_ok_test() ->
    Shapes = [{circle, 3}, {rectangle, 3, 4}],
    MathServer = start(),
    
    MathServer ! {request, self(), Shapes},
   
    receive
	M ->
	    ?assertEqual({MathServer, ok, 40.27433388230814}, M)
    end.

server_response_error_test() ->
    Shapes = [{ellipse, 3,1}],
    MathServer = start(),
    
    MathServer ! {request, self(), Shapes},
    
    receive
	M ->
	    ?assertEqual({MathServer, error, 0}, M)
    end.
