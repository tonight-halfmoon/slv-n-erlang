-module(integration_test).
-export([start/0, stop/0]).
-import(user_interface, [connect/0, allocate/0, free/1, stats/0, disconnect/0]).
-import(client, [client/1]).
-import(resa_server, [start/1]).
-include_lib("eunit/include/eunit.hrl").
-include("../config/config.hrl").
-include("../client/interface_client.hrl").
-include("../server/config_internal.hrl").
-include("../server/interface_server.hrl").
-include("../config/telecommunication.hrl").
-define(toyzres, 'ab.12.0').
-define(monitor_active, monitor_active).

%%% Experience:
%%% Integration Test monitors both RESA's nodes, in order to insure testing coverage is progressing on healthy run.

start() ->
    start_server_node(),
    start_server(),
    start_client_node(),
    connect_client(),
    eunit:test([?MODULE],[verbose]),
    ui_simulate_disconnect().

stop() ->
    resa_server:stop().

start_server_node() ->
    case net_kernel:start([?server, longnames]) of % shortnames
	{ok, Pid} ->
	    io:format("A distributed node ~p started with Pid ~p~n", [?server_node, Pid]);
	{error, {already_started, Pid}} ->
	    io:format("The distributed node ~p already started with Pid ~p~n", [?server_node, Pid])
    end.

start_server() ->
    case resa_server:start([?toyzres]) of
	{ok, Pid}->
	    io:format("RESA Server started with Pid ~p~n", [Pid]),
	    global:register_name(?server, Pid);
	{server_running, Pid} ->
	    io:format("RESA Server is running with Pid ~p~n", [Pid])
    end.

%%% Precondition: 
%%% Resa Server must have started, to allow monitoring its activity's status. 
%%% If you want to handle monitoring newly connected nodes, that needs another treatment.
start_client_node() ->
    net_kernel:start([?client_name, longnames]). % shortnames

connect_client() ->
   case user_interface:connect() of
       {ok, Pid} ->
	   io:format("Client ~p connnected. Pid is ~p~n", [?client_name, Pid]);
       {already_connected, Pid} ->
	   io:format("Client ~p already connnected. Pid is ~p~n", [?client_name, Pid])
       end.

ui_simulate_disconnect() ->
    Result = user_interface:disconnect(),
    io:format("~p~n", [Result]),
    ?assertEqual(disconnect, Result).

simulate_allocate_test_() ->
    R = ?handler ! #allocate_resource{server=?server, from_pid=whereis(?client_name)},
    ?_assertEqual({allocate_resource,?server, whereis(?client_name)}, R).

simulate_freeup_test_() ->
    R = ?handler ! #free_resource{server=?server, from_pid=whereis(?client_name), resource=term_to_binary(?toyzres)},
    ?assertNotEqual(undefined, whereis(?client_name)),
    {
      "When ?handler process receives free_resource message then it must receive it as expected.",
      ?_assertEqual({free_resource,resa_server, whereis(?client_name),<<131,100,0,7,97,98,46,49,50,46,48>>}, R)
    }.

ui_simulate_allocate_test_() ->
    Result = user_interface:allocate(),
    io:format("allocate result ~p~n", [Result]),
    {
      "User Interface Allocate must send the right protocol to Resa Server",
      ?_assertEqual(attempt2allocate, Result)
    }.

ui_simulate_free_test_() ->
    {
      "User Interface Free must send the right protocol to Resa Server",
      ?_assertEqual(#attempt2free{resource=?toyzres}, apply(user_interface, free, [?toyzres]))
    }.

ui_simulate_stats_test_() ->
    {
      "User Interface Stats must send the right protocol to Resa Server",
      ?_assertEqual(attempt4stats, user_interface:stats())
    }.
