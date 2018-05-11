-module(socket_example).
-export([nano_get_url/0]).
%% Why programming with sockets fun? Because it allows applications to interact with othjer machines on the Internet, which has far more potentialthan just performing local operations.

%% In this chapter, we'll see how to program client and servers using TCP and UDP sockets.
%% We'll go through the different forms of servers that are possible (parallel, sequential, blocking, and nonblocking) and see how to program traggic-shaping applications that can control the flow of data to the application.

nano_get_url() ->
    nano_get_url("www.google.de").

nano_get_url(Host) ->
    %% Open a TCP socket to port 80 of host by calling gen_tcp:connect
    %% The argument binary tells the system to open the socket in binary mode and deliver all data to the applicaion as binaries
    %% {packet,0} means the TCP data is delivered directly to the applicaion in an unmodified form.
    {ok, Socket} = gen_tcp:connect(Host, 80, [binary, {packet, 0}]),
    %% send the message GET / HTTP/1.0\r\n\r\n to the socket.
    ok = gen_tcp:send(Socket, "GET / HTTP/1.0\r\n\r\n"),
    receive_data(Socket, []).

receive_data(Socket, SoFar) ->
    %% the reply does not come all in one packet but comes fragmented, a bit a time.
    %% These fragments will be received as a sequence of messages that are sent to the process that opened (or controls) the socket.
    receive
	{tcp, Socket, Bin} ->
      %% We received a {tcp,Socket,Bin} message. The third argument in the tuple is a binary. This is because we opened the socket in binary
      %% mode. This message is on of the data fragments we have received so far and wait for the next fragment.
	    receive_data(Socket, [Bin|SoFar]);
	{tcp_closed, Socket} ->
      %% We received a {tcp_closed,Socket} message. This happens when the server has finished sending us data.
	    list_to_binary(reverse(SoFar))
    end.

reverse(L) ->
    reverse(L,[]).
reverse([], R)->R;
reverse([H|T], R) ->
    reverse(T, [H|R]).
