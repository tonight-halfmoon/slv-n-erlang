-module(resa_client).
-export([client/1]).
-include("resa_config.hrl").
-include("client_interface.hrl").
-include("resa_intercommunicate.hrl").

client(Server_node) ->
    {?server, Server_node} ! #connect{client_pid=self()},
    await_result(),
    interact(Server_node).

interact(Server_node) ->
    receive
	attempt2allocate ->
	    {?server, Server_node} ! #cask2alloc{client_pid=self()},
	    await_result();
	#attempt2free{resource=Resource} ->
	    {?server, Server_node} ! #cask2free{client_pid=self(), resource=Resource},
	    await_result();
	attempt4stats ->
	    {?server, Server_node} ! #cask4stats{client_pid=self()},
	    await_result();
	disconnect ->
	    exit(normal)
    end,
    
    interact(Server_node).

await_result() ->
    receive
	#abort_client{message=Why} ->
	    io:format("~p~n", [Why]),
	    exit(normal);
	#server_reply{message=What} ->
	    io:format("~p~n", [What]),
	    What;
	#stats_reply{stats_free=#stats{name=Free, length=FL}, stats_allocated=#stats{name=Allocated, length=AL}} ->
	    io:format("~p~n", [{{Free, FL},{Allocated, AL}}]),
	    {{Free, FL},{Allocated, AL}}

    after 5000 ->
	    io:format("No response from server~n", []),
	    exit(timeout)
    end.
