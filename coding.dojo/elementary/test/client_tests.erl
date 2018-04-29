-module(client_tests).
-include_lib("eunit/include/eunit.hrl").

reply_ok_test() ->
    MathServerPid = math_server:start(),
    Shapes =  [{circle, 3}, {rectangle, 3, 4}],
    ClientPid = client:start(MathServerPid),
    
    client:request(ClientPid, Shapes),
    Response = receive M -> M end,
    ?assertEqual({response, ok, 40.27433388230814}, Response).

%reply_error_test() ->
%    MathServer = math_server:start(),
%    Shapes =  [{ellipse, 3, 1}],
%    
%    Response = math_user_interface:request_server(MathServer, Shapes),
%    
%    ?assertMatch({response, error, M} when not is_float(M), Response).

