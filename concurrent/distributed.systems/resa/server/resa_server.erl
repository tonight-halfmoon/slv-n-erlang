-module(resa_server).
-export([start/1, stop/0, server/0]).
-include("../config/config.hrl").
-include("config_internal.hrl").
-include("interface_server.hrl").
-include("interface_provider.hrl").
-include("../config/telecommunication.hrl").
-import(handler, [handle/2]).
-import(stats_provider, [mk_stats/0]).
-define(all_registered, [?server, ?handler, ?stats]).

%%% Knowledge
%%% A single server may actually be a large network of communicating processes
%%% which implement a service, all of which would be hidden from the user by
%%% the interface functions. It is the set of interface functions which should
%%% be published, thiat is to say made available to users, as hese functions provide
%%% the only legal means of accessing the services provided by a server. [Reference: Concurrent Programming in Erlang, Joe Armstrong et. al., 2nd Edt., Ericsson].

start(Free) ->
    case whereis(?server) of
	undefined ->
	    register(?server, spawn(?MODULE, server, [])), 
	    register(?handler, spawn(handler, handle, [Free, []])),
	    register(?stats, spawn(stats_provider, mk_stats, []));
	  _ ->
	    server_running
    end.

stop() ->
    case whereis(?server) of 
	undefined ->
	    server_not_running;
	_ ->
	    unregister_all(?all_registered)
    end.

unregister_all([]) ->
    true;
unregister_all([H|T]) ->
    case whereis(H) of 
	undefined ->
	    unregister_all(T);
	_ ->
	    unregister(H),
	    unregister_all(T)
    end.

%%% Knowledge
%%% The interface functions.
%%% The purpose of the interface functions is to create abstractions which hide
%%% the specific details of the protocols used between the clients and the server.
%%% A user of a service does not need to know the details of the protocols uesd 
%%% to implement the service, or the internal data structures and algorithms
%%% used in the server. An implementor of the service is then free to change any
%%% of these internal details at any time while maintaining the same user
%%% interface. [Reference: Concurrent Programming in Erlang, Joe Armstrong et. al., 2nd Edt., Ericsson].
%%%
%%% Experience: check user_interface.erl

server() ->
    process_flag(trap_exit, true),
    active().

%%% Knowledge
%%% The process which replies to the server request may not 
%%% be the actual server itself, but a different process to which the request
%%% has been delegated. [Reference: Concurrent Programming in Erlang, Joe Armstrong et. al., 2nd Edt., Ericsson].
%%% 
%%% And, once again, we hide the protocol of how server communicate with handlers and providers
active() -> %%% Server wouldn't bother to deal with data
    receive %%% Experience
            %%% See the flexibility here! Handling the server request is delegated
            %%% to another process without a need to change the interface functions.
	#connect{client_pid=FromPid} ->
	    connect_client(FromPid);
	{'EXIT', FromPid, _} ->
	    disconnect_client(FromPid);
	#cask2alloc{client_pid=FromPid} ->
	    ?handler ! #allocate_resource{server=?server, from_pid=FromPid},
	    await_handler(FromPid);
	#cask2free{client_pid=FromPid, resource=Resource} ->
	    ?handler ! #free_resource{server=?server, from_pid=FromPid, resource=Resource},
	    await_handler(FromPid);
	#cask4stats{client_pid=FromPid} ->
	    ?handler ! #server_request_data{server=?server},
	    await_handler(FromPid);
	{From, Msg} ->
	    io:format("Received ~p from ~p~n", [From, Msg]),
	    unregister_all(?all_registered),
	    From ! {reply, node(), nothing_special};
	_ ->
	    unregister_all(?all_registered),
	    not_interested
    end,
    active().

await_handler(FromPid) ->
    receive
	#handler_reply_data{data=#data_structure{free=Free, allocated=Allocated}} ->
	    ?stats ! #request_stats{from_pid=FromPid, free=Free, allocated=Allocated};
	#handler_reply{message=Message} ->
	    FromPid ! #server_reply{message=Message};
	#handler_refused{reason=Reason} ->
	    FromPid ! #server_reply{message=lists:concat([request_not_carried_out, Reason])}
    end.

connect_client(ClientPid) ->
    ClientPid ! #server_reply{message=connected},
    link(ClientPid),
    io:format("Client ~p connected.~n", [ClientPid]),
    true.

disconnect_client(FlientPid) ->
    FlientPid ! #abort_client{message='no_specific_reason!'},
    true.
