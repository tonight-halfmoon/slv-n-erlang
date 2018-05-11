-module(tcp_server).
-export([server/1]).

server(Port) ->
    case gen_tcp:listen(Port, [binary, {packet, 0}, 
				{active, false}]) of %% need to invoke recv/2 
	{ok, ListenSocket} ->
	    {ok, LPort} = inet:port(ListenSocket), 
	    io:format("[S] Server is listening on Port ~p. Listen Socket: ~p~n", [LPort, ListenSocket]),
	    case gen_tcp:accept(ListenSocket) of 
		{ok, Socket} ->
		    io:format("[S] Server accepted a connection from Socket ~p on Listen Socket: ~p ~n", [Socket, ListenSocket]),
		    {ok, Bin} = do_recv(Socket, []),
		    io:format("Message received ~p~n", [Bin])
		    %send_message(Socket),
		    %ok = gen_tcp:close(Socket)
			;
		{error, Reason} ->
		    io:format("[S] Server did not accept a socket because ~p~n", [Reason])
	    end 
	  	;
	{error, Reason} ->
	    io:format("[S] Server failed to listen on Port ~p, because ~p~n", [Port, Reason])
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


send_message(Socket) ->
    %Bin = list_to_binary("Goodbye from Erlang Server"),
    Message = "Message from Erlang",
    case gen_tcp:send(Socket, Message) of
	ok ->
	    io:format("Message '~p' send ~n", [Message]);
	{error, Reason} ->
	    io:format("Message not sent. Reason ~p~n", [Reason])
    end.
