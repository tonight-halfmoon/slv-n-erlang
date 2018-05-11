-module(tcp_client).
-export([client/1, client/2, client2/1]).

client(Port) ->
    client("localhost", Port).

client(Address, Port) ->
    {ok, Socket} = gen_tcp:connect(Address, Port, [binary, {packet, 0}]),
    Packet_2send=  "MessageXXXX",
    ok = gen_tcp:send(Socket, Packet_2send),
    ok = gen_tcp:close(Socket),
    ok.

client2(Port) ->
    client2("localhost", Port).
client2(Address, Port) ->
    case gen_tcp:connect(Address, Port, [binary, {packet, 0}]) of
	{ok, Socket} ->
	    io:format("*** Connection established. Socket: ~p~n", [Socket]),
	    send_message(Socket),
	   % Response = do_recv(Socket, []),
	   % Response =  gen_tcp:recv(Socket, 0),
	   % io:format("Response from Server: ~p~n", [Response]),
	    ok = gen_tcp:close(Socket)
		;
	{error, Reason} -> %% Reason: closed|timeout|system_limit| inet:posix()
	    io:format("*** Connection not established. Reason: ~p~n", [Reason])
    end,
    ok.

send_message(Socket) ->
    Message =  "Message from Client", %% Packet to send
    case gen_tcp:send(Socket, Message) of 
	ok ->
	    io:format("*** Message '~p' sent.~n", [Message]);
	{error, Reason} ->
	    io:format("*** Message not sent. Reason: ~p~n", [Reason])
		%;
	%{tcp, Socket, Data} ->
	    %io:format("received response from server: ~p~n", [Data])
    end,
    ok.



do_recv(Socket, Packets) ->
    case gen_tcp:recv(Socket, 0) of %% recv/2 is invoked to retrieve the packets because Listen Socket is to {active, false}
	{ok, Packet} ->
	    do_recv(Socket, [Packets, Packet]);
	{error, Reason} ->
	    io:format("[S.recv] Socket ~p~n.", [Reason]),
	    {ok, list_to_binary(Packets)}
    end.
	 
