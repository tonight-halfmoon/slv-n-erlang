-module(client).
-export([client/1]).
-include("../config/config.hrl").
-include("interface_client.hrl").
-include("../config/telecommunication.hrl").

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
	    case screening(Resource) of
		ok ->
		    {?server, Server_node} ! #cask2free{client_pid=self(), resource=term_to_binary(Resource)},
		    await_result();
		_ ->
		    io:format("Unacceptable data type provided~n", []),
		    true
	    end;
	attempt4stats ->
	    {?server, Server_node} ! #cask4stats{client_pid=self()},
	    await_result();
	disconnect ->
	    exit(normal)
    end,
    interact(Server_node).

%%% Experience
%%% Obvious! Client has no knowledge about the real data structure of the server.

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

%%% Experience
%%% No one will be happy with a request against the server carrying unchecked data.

screening(R) when not is_atom(R) ->
    unacceptable;
screening(_) ->
    ok.
