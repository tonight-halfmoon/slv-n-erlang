-module(user_interface).
-export([request_server/2]).

request_server(Server, Request) ->
    spawn(fun() -> request(Server, Request) end).

request(Server, Request) ->
    Server ! {request, self(), Request},
    receive
	    {math_server, ok, Reply} ->
	    	io:format("~p received from Math Server: ~p~n", [self(), Reply]);
	    Message ->
	    	io:format("~p received: ~p~n", [self(), Message])
	end.

% extra evaluation on Emulator
% Eshell V9.3  (abort with ^G)
% 1> math_server:start().
% true
% 2> user_interface:request_server(math_server, [{circle, 3}]).
% <0.65.0> received reply from Math Server 28.274333882308138
% ok
