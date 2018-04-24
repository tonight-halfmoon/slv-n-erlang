-module(client).
-export([request/2]).

request(Server, Query) ->
    Server ! {request, self(), Query},
    receive
	    {math_server, ok, Reply} ->
	    	io:format("Client received reply from Math Server ~p~n", [Reply]);
	    Message ->
	    	io:format("Client received message: ~p~n", [Message])
	end.

% extra evaluation on Emulator
% Eshell V9.3  (abort with ^G)
% 1> math_server:start().
% true
% 2> client:request(math_server, [{circle, 3}]).
% Client received reply from Math Server 28.274333882308138
% ok
