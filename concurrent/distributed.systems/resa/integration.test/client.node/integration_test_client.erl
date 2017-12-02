-module(integration_test_client).
-export([start/0, run/0, stop/0]).
-import(user_interface, [connect/0, allocate/0, free/1, stats/0, disconnect/0]).
-import(client, [client/1]).
-import(node_lib, [start_node/3]).
-include_lib("eunit/include/eunit.hrl").
-include("../../config/config.hrl").
-include("../../client/interface_client.hrl").
-include("../../config/telecommunication.hrl").
-define(resource_example, 'ab.12.0').

%%% Future Work:
%%% Integration Test monitors both RESA's nodes, in order to insure testing coverage is progressing on healthy run.

%%% To simulate Client/Server interaction and test the API and Protocols with Integration Testing
%%% 1) start integration test server node
%%% 2) start integration test client_node (this module)
%%% 3) run in any order this module and integration test server node
%%% 4) when done stop the module and then the integration test server node.

%%% Precondition:
%%% server_node of Integration Test has started.

start() ->
    start_client_node(),
    connect_client().

%%% Precondition:
%%% server_node of Integration Test has started.

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
    {
      "User Interface's function 'Aalocate' must send the right protocol to the server",
      ?_assertEqual(attempt2allocate, user_interface:allocate())
    }.

ui_freeup_test_() ->
    {
      "User Interface's function 'free' must send the right protocol to the server",
      ?_assertEqual(#attempt2free{resource=?resource_example}, user_interface:free(?resource_example))
    }.

ui_stats_test_() ->
    {
      "User Interface's function 'stats' must send the right protocol to Resa Server",
      ?_assertEqual(attempt4stats, user_interface:stats())
    }.

%%% Integration Test's Client node acts as a client and overrides published API by requesting services from the server using protocols.
%%% Overriding the API is for the purpose of Integration Testing.

client_ask4stats_test_() ->
    {?server, ?server_node} ! #cask4stats{client_pid=self()},
    receive
	M = #stats_reply{stats_free=#stats{name=_, length=FL}, stats_allocated=#stats{name=_, length=AL}}  ->
	    {
	      "When Client asks the server for stats on resources, then the server must delegate the request to the stats provider which in turn it must reply to the client",
	      ?_assertMatch({stats_reply, {stats, free, FL},{stats, allocated, AL}} when {true, true} =:= {is_integer(FL), is_integer(AL)}, M)
	    }
    end.

client_ask2freeup_test_() ->
    {?server, ?server_node} ! #cask2alloc{client_pid=self()},
    receive
	_ ->
	    ok
    end,
    {?server, ?server_node} ! #cask2free{client_pid=self(), resource=term_to_binary(?resource_example)},
    receive
	M = #server_reply{message=_} ->
	    {
	      "When Client asks the server to free up a resource and that resource is allocated, then the server must free up the resource and reply with 'freed'.", 
	      ?_assertEqual({server_reply, {freed, ?resource_example}}, M)
	    }
    end.

client_ask2allocate_test_() ->
    {?server, ?server_node} ! #cask2free{client_pid=self(), resource=term_to_binary(?resource_example)},
    receive
	_ ->
	    ok
    end,
    {?server, ?server_node} ! #cask2alloc{client_pid=self()},
    receive
	M = #server_reply{message=_} ->
	    {
	      "",
	      ?_assertEqual({server_reply, {allocated, ?resource_example}}, M)
	    }
    end.
