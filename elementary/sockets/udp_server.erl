-module(udp_server).
-export([start_server/0]).


%% 5> udp_server:start_server().
%% <0.78.0>
%%server opened socked:#Port<0.2123>
%%server received:{udp,#Port<0.2123>,{127,0,0,1},51539,<<131,97,40>>}

start_server() ->
    spawn(fun() -> server(4000) end).

%% The server
server(Port) ->
    {ok, Socket} = gen_udp:open(Port, [binary]),
    io:format("server opened socket:~p~n", [Socket]),
    loop(Socket).

loop(Socket) ->
    receive
	{udp, Socket, Host, Port, Bin} = Msg ->
	    io:format("server received:~p~n", [Msg]),
	    {Ref, N} = binary_to_term(Bin),
	    Fac = fac(N),
	    io:format("factorial of ~p is ~p", [N,Fac]),
	    gen_udp:send(Socket, Host, Port, term_to_binary({Ref, Fac})),
	    loop(Socket)
    end.
fac(0) ->
    1;
fac(N) ->
    fac(N, 1).
fac(1, Acc) -> Acc;
fac(N, Acc) -> fac(N-1, N *Acc).


