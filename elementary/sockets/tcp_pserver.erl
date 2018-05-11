%%%
%%%   TCP Parallel Server
%%%
%%%  
%%% Munich, May 2017  
%%%
-module(tcp_pserver).
-export([start/0, start/2]).
-include_lib("eunit/include/eunit.hrl").
%% To write this program (and indeed any program that runs over TCP/IP), 
%% we have to answer a few simple questions:
%%    - How is the data organised? How do we know how much data makes up a single request or response?
%%    - How is the data within a request or the response encoded and decoded?

%% {packet, N}, e.g., 1,2,4. 
%% I made a little experiment to send a zero-b1te message using
%% dd if=/dev/zero bs=9000 count=1000 > /dev/tcp/localhost/2345
%% dd if=/dev/zero > /dev/tcp/localhost/2345
%% and set the gen_tcp server to accept 0-byte fragment with {packet, 0}

%% Moreover, I was able also to something when I used curl command as folllows
%% curl http://localhost:2345, simply!

start() ->
    %% Let's have it our default
    start_parallel_server(2345, 4, true).

start(Port, Fragment_size) ->  
    start_parallel_server(Port, Fragment_size, once).

%% client and server must agree on the argument to packet used with {packet, N}
start_parallel_server(Port, Fragment_size, Active) ->
    %% possible data type options: binary | list. 
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {packet, Fragment_size}, {reuseaddr, true}, {active, Active}]),
       %% The difference between active and passive sockets has to do with what happens when messages are received by the socket.
%% Passive sockets are used to control the flow of data to a server.
					 % {active, once}]),

    %% if the controlling process dies, then socket will be closed
    spawn(fun() -> connect(ListenSocket) end).
    
connect(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> connect(ListenSocket) end),
    loop(Socket).

loop(Socket)->
    receive
	{tcp, Socket, RequestMsg} ->
	    Tokenized = consume(RequestMsg, Socket),
	    %Str = binary_to_term(BinSame),
	    %io:format("Server (unpacked) ~p~n", [Str]),
	    %Reply = lib_misc:string2value(Str),
	    Reply = handle(Tokenized, Socket),
	    io:format("Server replying: ~p~n", [Reply]),
	    gen_tcp:send(Socket, list_to_binary(Reply)),
	    %% configure partial-blocking
	    inet:setopts(Socket, [{active, once}]),
	    gen_tcp:close(Socket),
	    loop(Socket);
        {tcp_closd, Socket} ->
	    io:format("Server socket closed~n")
	end.
    
%% @spec inet:peername(Socket) -> {ok, {IP_Address, Port}} | {error, Why}

%% now with this pattern matching 
%% the server can accept {packet, 4} with zero-byte message using 
%% dd if=/dev/zer count =1 > /dev/tcp/localhost/2345

consume(Msg, Socket) ->
    io:format("Received Message  ~p~n ",[Msg]),
    io:format("From: ~p~n", [inet:peername(Socket)]),
    consume(Msg).

consume(<<>>) ->
    io:format(".. which is empty <<>> ~n"),
    term_to_binary("{}");
%consume([]) ->
%    io:format(".. which is empty list ~n"),
%    term_to_binary("{}");
consume(Bin) when is_binary(Bin) ->
    io:format(".. which is binary ~n"),
    [F,S|T] = string:tokens(binary_to_list(Bin), " \r\n\t"),
    io:format("Tokenised: ~p~n", [[F,S|T]] ),
    {F,S}.
%consume(List) when is_list(List) ->
%    io:format(".. which is a list ~n"),
%    list_to_binary(string:tokens(List,"\r\n"));

handle({"GET", Second_arg}, Socket) ->
    response(Second_arg, Socket)
	;
handle(_Unexpected, Socket) ->
    io:format("Not expected value of parameter in method 'handle': ~p~n", [_Unexpected]),
    response([], Socket).
    
response([], _Socket) ->
    "404 Not Found\n";
response(_Arg, _Socket) ->
    "Hello from your Erlang TCP Server\n".

start_zero_size_fragment_parallel_server_test() ->
  %%start_parallel_server(2345, 0, once).

    %% Client GET: curl http://localhost:2345
    %% client POST: curl -d @payload.md  -H "Content-Type: multipart/form-data" http://localhost:2345
    %% Client POST json string:
    %% $ curl -d '{"Hello":"Erlang!"}' -H "Content-Type: application/x-www-form-urlencoded" http://localhost:2345

    %% TCP Server: Msg received: "POST / HTTP/1.1\r\nHost: localhost:2345\r\nUser-Agent: curl/7.54.0\r\nAccept: */*\r\n\r\n"
      ?assertEqual(1,1).

