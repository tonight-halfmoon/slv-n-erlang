-module(math_server_tests).
-include_lib("eunit/include/eunit.hrl").
-import(math_server, [start/0]).

start_ok_test() ->
    MathServerPid = start(),
    
    ?assertMatch(Pid when is_pid(Pid), MathServerPid),
    ?assertEqual(true, is_process_alive(MathServerPid)).

response_ok_test() ->
    Shapes = [{circle, 3}, {rectangle, 3, 4}],
    MathServerPid = start(),
    
    MathServerPid ! {request, self(), Shapes},
   
    receive
	Response ->
	    ?assertEqual({MathServerPid, ok, 40.27433388230814}, Response)
    end.

%response_error_test() ->
%    Shapes = [{ellipse, 3,1}],
%    MathServer = start(),
%    
%    MathServer ! {request, self(), Shapes},
    
%    receive
%	Response ->
%	    ?assertEqual({MathServer, error, 0}, Response)
%   end.
