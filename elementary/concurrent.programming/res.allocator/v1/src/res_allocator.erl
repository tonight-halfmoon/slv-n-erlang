-module(res_allocator).
-export([start/1, stop/0, server/0, allocate/0, free/1, stats/0]).
-include("resalloc_config.hrl").
-include("resalloc_interface.hrl").
-import(res_handler, [handle/2]).
-import(stats_provider, [mk_stats/0]).

%%% A single server may actually be a large network of communicating processes 
%%% which implement a service, all of which would be hidden from the user by
%%% the interface functions. It is the set of interface finctions which should 
%%% be published, thiat is to say made available to users, as hese functions provide
%%% the only legal means of accessing the services provided by a server.
start(Resources) ->
    case whereis(?server) of
	undefined ->
	    register(?server, spawn(?MODULE, server, [])), 
	    register(?handler, spawn(res_handler, handle, [Resources, []])),
	    register(?stats, spawn(stats_provider, mk_stats, []));
	  _ ->
	    server_running
    end.

stop() ->
    case whereis(?server) of 
	undefined ->
	    server_not_running;
	_ ->
	    unregister(?server),
	    unregister(?handler),
	    unregister(?stats),
	    ok
    end.

%%% The interface functions.
%%% The purpose of the interface functions is to create abstractions which hide
%%% the specific details of the protocols used between the clients and the server.
%%% A user of a service does not need to know the details of the protocols uesd 
%%% to implement the service, or the internal data structures and algorithms
%%% used in the server. An implementor of the service is then free to change any
%%% of these internal details at any time while maintaining the same user
%%% interface.

allocate() ->
    request(alloc). %% Protocol 'alloc' is hidden behind the interface

free(Resource) ->
    request({free, Resource}). %% Protocol 'free' is hidden behind the interface

stats() ->
    request(stats).  %% Protocol 'stats' is hidden behind the interface

request(Request) ->
    ?server ! {self(), Request},
    receive
	{?server, Reply} ->
	    Reply		;
	#stats_reply{stats_free=#stats{name=Free, length=FL}, stats_allocated=#stats{name=Allocated, length=AL}} ->
	    {{Free, FL},{Allocated, AL}}
    end.

%%% The process which replies to the server request may not 
%%% be the actual server itself, but a different process to which the request
%%% has been delegated.
%%% 
%%% And, once again, we hide the protocol of how server communicate with handlers and providers
server() -> %% Server won't bather to hold data
    receive
            %%% See the flexibility here! Handling the server request is delegated
            %%% to another process without a need to change the interface functions.
	{From, alloc} ->
	    %allocate(Free, Allocated, From);
	    %?handler ! {?server, From, alloc},
	    ?handler ! #allocate_resource{server=?server, from_pid=From},
	    server();
	{From, {free, Resource}} ->
	    %free(Free, Allocated, From, Resource)
	    %?handler ! {?server, From, {free, Resource}},
	    ?handler ! #free_resource{server=?server, from_pid=From, resource=Resource},
	    server();
	{From, stats} ->
	    %?handler ! {?server, ?handler, From, data},
	    ?handler ! #server_request_data{server=?server, from_pid=From},
	    ask4stats(),
	    server()
    end.

ask4stats() ->
    receive
	#handler_reply_data{from_pid=FromPid, data=#data_structure{free=Free, allocated=Allocated}} ->
	    ?stats ! #request_stats{from_pid=FromPid, free=Free, allocated=Allocated}	
    end.
 
