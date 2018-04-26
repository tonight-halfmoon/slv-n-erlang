-module(math_user_interface_tests).
-include_lib("eunit/include/eunit.hrl").

receive_response_ok_test() ->
    MathServer = math_server:start(),
    Shapes =  [{circle, 3}, {rectangle, 3, 4}],
    
    Response = math_user_interface:request_server(MathServer, Shapes),
    
    ?assertEqual({response, ok, 40.27433388230814}, Response).

dealwith_when_server_has_shutdown_ok_test() ->
	Pid = list_to_pid("<0.34.0>"),
	?assertNot(is_process_alive(Pid)),
	Shapes = [{circle, 3}],

	case catch math_user_interface:request_server(Pid, Shapes) of
		Timeout ->
		 	?assertEqual({'EXIT',timeout}, Timeout)
	end.

