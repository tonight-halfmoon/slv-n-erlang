-module(integration_test_client).
-export([start/0, run/0, stop/0]).
-import(user_interface, [connect/0, allocate/0, free/1, stats/0, disconnect/0]).
-import(client, [client/1]).
-import(node_lib, [start_node/3]).
-include_lib("eunit/include/eunit.hrl").
-include("../../config/config.hrl").
-include("../../client/interface_client.hrl").
-define(resource_example, 'ab.12.0').

%%% Future Work:
%%% Integration Test monitors both RESA's nodes, in order to insure testing coverage is progressing on healthy run.

start() ->
    start_client_node(),
    connect_client().

run() ->
    eunit:test([?MODULE],[verbose]).

stop() ->
    disconnect_client(),
    net_kernel:stop().

%%% Precondition: 
%%% Resa Server must have started on another node, to allow monitoring its activity's status. 
%%% If you want to handle monitoring newly connected nodes, that needs another treatment.
start_client_node() ->
   node_lib:start_node(?client_name, ?client_node, longnames).

connect_client() ->
   case user_interface:connect() of
       {ok, Pid} ->
	   global:register_name(?client_name, Pid, fun global:notify_all_name/3),
	   io:format("Client ~p connnected. Pid is ~p~n", [?client_name, Pid]);
       {already_connected, Pid} ->
	   io:format("Client ~p already connnected. Pid is ~p~n", [?client_name, Pid])
       end.

disconnect_client() ->
    case user_interface:disconnect() of
	already_disconnected ->
	    io:format("Client already disconnected~n", []);
	disconnect ->
	   true
    end.

ui_allocate_test_() ->
    Result = user_interface:allocate(),
    io:format("allocate result ~p~n", [Result]),
    {
      "User Interface Allocate must send the right protocol to Resa Server",
      ?_assertEqual(attempt2allocate, Result)
    }.

ui_freeup_test_() ->
    {
      "User Interface Free must send the right protocol to Resa Server",
      ?_assertEqual(#attempt2free{resource=?resource_example}, apply(user_interface, free, [?resource_example]))
    }.

ui_stats_test_() ->
    {
      "User Interface Stats must send the right protocol to Resa Server",
      ?_assertEqual(attempt4stats, user_interface:stats())
    }.
