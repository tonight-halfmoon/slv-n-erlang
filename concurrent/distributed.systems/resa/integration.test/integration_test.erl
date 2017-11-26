-module(integration_test).
-export([run/0, cleanup/0, start_server_atnode/0, start_client_node/0, client_node_active/1, node_active/1,
	 ui_simulate_connect/0, ui_simulate_disconnect/0]).
-import(user_interface, [connect/0, allocate/0, free/1, stats/0, disconnect/0]).
-import(client, [client/1]).
-import(resa_server, [start/1]).
-import(monitor_resa_nodes, [monitor/1]).
-include_lib("eunit/include/eunit.hrl").
-include("../config/config.hrl").
-include("../client/interface_client.hrl").
-include("../config/telecommunication.hrl").
-define(toyzres, 'ab.12.0').
-define(monitor_active, monitor_active).
-define(client_node, resa_client_node).

%%% Experience:
%%% Integration Test monitors both RESA's nodes, in order to insure testing coverage is progressing on healthy run.

run() ->
    start_server_atnode(),
    start_client_node(),
    ui_simulate_connect(),
    eunit:test([?MODULE],[verbose]),
    ui_simulate_disconnect(),
    cleanup().

cleanup() ->
    case is_alive(?client_node) of
	true ->
	    unregister(?client_node);
	false ->
	    ok
	end,
    case is_alive(?monitor_active) of
	true ->
	    unregister(?monitor_active);
	false ->
	    ok
    end,
    resa_server:stop(),
    true.

start_server_atnode() ->
    net_kernel:stop(),
    net_kernel:start([?server, names]), % shortnames
    ?assertEqual(true, resa_server:start([?toyzres])),
    true.

%%% Precondition: 
%%% Resa Server must have started, to allow monitoring its activity's status. 
%%% If you want to handle monitoring newly connected nodes, that needs another treatment.
start_client_node() ->
    net_kernel:stop(),
    net_kernel:start([?client_node, names]), % shortnames
    register(?monitor_active, spawn(?MODULE, client_node_active, [self()])).

client_node_active(Pid) ->
    process_flag(trap_exit, true),
    monitor_resa_nodes:monitor(monitor_active),
    node_active(Pid).

node_active(Pid) ->
    receive
	{'EXIT', FromPid, Reason} ->
	    io:format("Received EXIT flag from ~p, for Reason ~p~n", [FromPid, Reason]),
	    exit(normal);
	server_node_down ->
	    io:format("Server node is down~n", []),
	    net_kernel:stop(),
	    Pid ! exit,
	    exit(normal);
	What ->
	    io:format("'client_node_active' process received: ~p~n", [What])
   end,
   node_active(Pid).

is_alive(Registered) ->
    case whereis(Registered) of
	undefined ->
	    io:format("resa_client is undefined~n", []),
	    false;
	_ ->
	    io:format("resa_client is alive~n", []),
	    true
    end.

ui_simulate_connect() ->
    ?assertEqual(false, is_alive(?client_name)), % otherwise, we have unclean stop from a former run.
    Result = user_interface:connect(),
    Client_pid = whereis(?client_name),
    ?assertEqual(true, Result),
    ?assertEqual(true, is_alive(?client_name)),
    %global:register_name(resa_client, Client_pid),
    %Client_pid = global_group:whereis_name(resa_client),
    io:format("Client pid of resa_client is ~p~n", [Client_pid]).

ui_simulate_disconnect() ->
    %%io:format("~p~n", [global_group:send(resa_client, disconnect)]).
    Result = user_interface:disconnect(),
    io:format("~p~n", [Result]),
    ?assertEqual(disconnect, Result).

ui_simulate_allocate_test_() ->
    Result = user_interface:allocate(),
    io:format("allocate result ~p~n", [Result]),
    {"User Interface Allocate must send the right protocol to Resa Server", ?_assertEqual(attempt2allocate, Result)}.
 
ui_simulate_free_test_() ->
   {"User Interface Free must send the right protocol to Resa Server", ?_assertEqual(#attempt2free{resource=?toyzres}, apply(user_interface, free, [?toyzres]))}.

ui_simulate_stats_test_() ->
    {"User Interface Stats must send the right protocol to Resa Server", ?_assertEqual(attempt4stats, user_interface:stats())}.
