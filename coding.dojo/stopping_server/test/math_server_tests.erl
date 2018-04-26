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
	Response ->
	    ?assertEqual({MathServer, ok, 40.27433388230814}, Response)
    end.

server_stop_when_receive_stop_request_ok_test() ->
	MathServer = start(),
	
	?assert(is_process_alive(MathServer)),

	MathServer ! stop,

	receive
	after 50 -> ok end,

	?assertNot(is_process_alive(MathServer)).

