-module(user_interface).
-export([allocate/0, free/1, stats/0, connect/0, disconnect/0]).
-include("interface_client.hrl").
-include("config.hrl").

connect() ->
    case whereis(?client_name) of 
	undefined ->
	    register(?client_name, spawn(client, client, [?server_node_short])); %% short names vs. fully qualified names. Dynamic configuration /check at runtime needed. Check Integration Test. 
	_ ->
	    already_connected
    end.

disconnect() ->
    case whereis(?client_name) of
	undefined ->
	    already_disconnected;
	_ ->
	    ?client_name ! disconnect
    end.

allocate() ->
    case whereis(?client_name) of
	undefined ->
	    disconnected;
	_ ->
	    ?client_name ! attempt2allocate
    end.

free(Resource) ->
    case whereis(?client_name) of
	undefined ->
	    disconnected;
	_ ->
	    ?client_name ! #attempt2free{resource=Resource}
    end.

stats() ->
    case whereis(?client_name) of
	undefined ->
	    disconnected;
	_ ->
	    ?client_name ! attempt4stats
    end.
