-module(user_interface).
-export([start_client/1]).

start_client(Server) ->
    spawn(fun() -> loop(Server) end).

loop(Server) ->
    receive
	{request, Request} ->
	    Server ! {request, self(), Request},
	    loop(Server);
	{Server, ok, Reply} ->
	    io:format("~p received ~p from Server ~p~n.", [self(), Reply, Server]),
	    loop(Server);
	{Pid, ok, Message} ->
	    io:format("~p received ~p from ~p~n.", [self(), Message, Pid]),
	    loop(Server)
    end.
