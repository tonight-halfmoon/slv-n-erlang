-module(integration_test_server).
-export([start/0, run/0, stop/0]).
-import(user_interface, [connect/0, allocate/0, free/1, stats/0, disconnect/0]).
-import(client, [client/1]).
-import(resa_server, [start/1]).
-import(node_lib, [start_node/3]).
-include_lib("eunit/include/eunit.hrl").
-include("../../config/config.hrl").
-include("../../client/interface_client.hrl").
-include("../../server/config_internal.hrl").
-include("../../server/interface_server.hrl").
-include("../../config/telecommunication.hrl").
-include("../../server/interface_provider.hrl").
-define(resource_example, 'ab.12.0').

%%% Future Work:
%%% Integration Test monitors both RESA's nodes, in order to insure testing coverage is progressing on healthy run.

%%% To simulate Client/Server interaction and test the API and Protocols with Integration Testing
%%% 1) start integration test server node (this module)
%%% 2) start integration test client_node
%%% 3) run in any order this module and integration test client node
%%% 4) when done stop the integration test client node and then this module.

start() ->
    start_server_node(),
    start_server().

%%% Precondition:
%%% client_node of Integration Test has started.
run() ->
    eunit:test([?MODULE],[verbose]).

stop() ->
    resa_server:stop(),
    net_kernel:stop().

start_server_node() ->
    node_lib:start_node(?server, ?server_node, longnames).

start_server() ->
    case resa_server:start([?resource_example]) of
	{ok, Pid}->
	    io:format("RESA Server's Pid: ~p~n", [Pid]),
	    global:register_name(?server, Pid);
	{server_running, Pid} ->
	    io:format("RESA Server is running with Pid ~p~n", [Pid])
    end.

whereis_client() ->
    P = global:whereis_name(?client_name),
    ?assertNotEqual(undefined, P),
    P.

handler_allocate_test_() ->
    R = ?handler ! #allocate_resource{server=?server, from_pid=whereis_client()},
    {
      "When 'Data Handler' receives 'allocate_resource' message, and there is at least one free resource, then it must allocate the free resource.",
      ?_assertEqual({allocate_resource,?server, whereis_client()}, R)
    }.

handler_freeup_test_() ->
    R = ?handler ! #free_resource{server=?server, from_pid=whereis_client(), resource=term_to_binary(?resource_example)},
    ?assertNotEqual(undefined, whereis_client()),
    {
      "When 'Data Handler' process receives free_resource message, and the provided resource is allocated, then it must free up the resource.",
      ?_assertEqual({free_resource, resa_server, whereis_client(),<<131,100,0,7,97,98,46,49,50,46,48>>}, R)
    }.
