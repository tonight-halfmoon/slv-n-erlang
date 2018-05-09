-module(user_interface).
-export([allocate/0, free/1, stats/0, connect/0, disconnect/0]).
-include("interface_client.hrl").
-include("../config/config.hrl").
-import(client, [rpc/2]).

connect() ->
    case whereis(?client_name) of
	undefined ->
	    register(?client_name, spawn(client, client, [?server_node])),
	    {ok, whereis(?client_name)};
	_ ->
	    {already_connected, whereis(?client_name)}
    end.

disconnect() ->
    gw(disconnect).

allocate() ->
    gw(attempt2allocate).

free(Resource) ->
    gw(#attempt2free{resource=Resource}).

stats() ->
    gw(attempt4stats).

gw(Procedure) ->
    case whereis(?client_name) of
	undefined ->
	    already_disconnected;
	_ ->
	    rpc(?client_name, Procedure)
     end.
