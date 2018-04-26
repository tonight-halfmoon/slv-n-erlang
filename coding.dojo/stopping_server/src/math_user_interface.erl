-module(math_user_interface).
-export([request_server/2, stop_server/1]).

request_server(Server, Request) ->
    Server ! {request, self(), Request},
    receive
	{Server, ok, Reply} ->
	    {response, ok, Reply};
	M ->
	    {response, error, M}
    after 2000 ->
	exit(timeout)		      
    end.

stop_server(Server) ->
    Server ! stop.

