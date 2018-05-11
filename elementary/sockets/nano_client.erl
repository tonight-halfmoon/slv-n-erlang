-module(nano_client).
-export([nano_client_eval/1]).

%% how to run: 
%% run the tcp server on port 2345
%% nano_client:nano_client_eval("list_to_tuple([2+3*4,10+20])").
%% Another example, empty binary data
%% nano_client:nano_client_eval("{}").

nano_client_eval(Str) ->
    {ok, Socket} =
	gen_tcp:connect("localhost", 2345,
			[binary, {packet, 4}]),
    ok = gen_tcp:send(Socket, term_to_binary(Str)),
    receive
	{tcp, Socket, Bin} ->
	    io:format("Client received binary = ~p~n", [Bin]),
	    Val = binary_to_term(Bin),
	    io:format("Client result = ~p~n", [Val]),
	    gen_tcp:close(Socket)
	end.
