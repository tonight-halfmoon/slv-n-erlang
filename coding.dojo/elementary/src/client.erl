-module(client).
-export([request/2]).

request(ServerPid, Query) ->
    ServerPid ! {request, self(), Query},
    receive
	Message ->
	    io:format("Client received ~p~n", [Message])
	end.

% receive
%	{ServerPid, ok, Reply} ->
%	    io:format("Client ~p received reply from Server ~p~n", [self(), Reply])
%   end.
