-module(user_interface).
-export([request_server/2]).

request_server(Server, Request) ->
    Server ! {request, self(), Request},
    receive
	{Server, ok, Reply} ->
	    io:format("~p received ~p~n", [self(), Reply])
	end.
