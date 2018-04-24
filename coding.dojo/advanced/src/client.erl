-module(client).
-export([connect/1, loop/1]).

connect(Server) ->
    spawn(?MODULE, loop, [Server]).

loop(Server) ->
    receive
	{request, Query} ->
	    Server ! {request, self(), Query},
	    loop(Server);
	{Server, ok, Message} ->
	    io:format("Client ~p Received Message ~p from Server ~p~n.", [self(), Message, Server]),
	    loop(Server);
	{Pid, ok, Message} ->
	    io:format("Client ~p Received Message ~p from Pid ~p~n.", [self(), Message, Pid]),
	    loop(Server)
	end.
