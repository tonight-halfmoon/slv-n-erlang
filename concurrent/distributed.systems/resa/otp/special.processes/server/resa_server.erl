-module(resa_server).
-export([start_link/1, stop/0]).
-export([init/2]).
-export([system_continue/3, system_terminate/4,
	write_debug/3,
	system_get_state/1, system_replace_state/2]).
-include("config_internal.hrl").
-include("../config/config.hrl").
-include("../config/telecommunication.hrl").
-import(handler, [init_dh/2]).
-import(stats_provider, [init_sp/1]).
-define(all_registered, [?server, ?handler, ?ssp]).

%%% Special Processes
%%% A Process that complies to the OTP design principles, without using a standard 
%%% behaviour. Such a process is to:
%%%    - Be started in a way that makes the process fit into a supervision tree
%%%    - Support the sys debug facilities
%%%    - Take care of system messages.
%%% System messages are messages with a special meaning, used in the supervision tree. Typical system messages are
%%% requests for trace output, and requests to suspend or resume process execution (used during release handling). Processes
%%% implemented using standard behaviours automatically understand these messages. [Reference: OTP System Documentatoin, Erricson Labs]

start_link(Free) ->
    case whereis(?server) of
	undefined ->
	    proc_lib:start_link(?MODULE, init, [self(), Free]);	   
	_ ->
	    {server_running, whereis(?server)}
    end.

init(Parent, Free) ->
    unregister_all(?all_registered),
    register(?server, self()),
    %  - with spawn_link Server will be notified once handler has exited. 
    %  - with proc_lib:start_link or proc_lib:spawn_link, information stored about the ancestors and inital call that is needed for a process in a supervision tree.
    case proc_lib:start_link(handler, init_dh, [self(), {Free, []}]) of
	{ok, Dh_pid} ->
	    register(?handler, Dh_pid);
	{error, Reason} ->
	    exit(Reason)
    end,   
    register(?ssp, proc_lib:spawn_link(stats_provider, init_sp, [self()])),

    Deb = sys:debug_options([statistics, trace]),
    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
			    ?MODULE, #server_started{pid=self(), name=?server, state=Free}),
    proc_lib:init_ack(Parent, {ok, self()}),
    process_flag(trap_exit, true),
    active(Parent, Deb2).

active(Parent, Deb) -> 
    receive 
	{system, From, Request} ->
	    ?handler ! #server_request_data{server=?server},
	    receive
		#handler_reply_data{data=#data_structure{free=Free, allocated=Allocated}} ->
		    sys:handle_system_msg(Request, From, Parent, ?MODULE, Deb, {Free, Allocated})
	    end;
	#connect{client_pid=FromPid} ->
	    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
				    ?MODULE, {in, #connect{client_pid=FromPid}, FromPid}),
	    Deb3 = connect_client(FromPid, Deb2),
	    active(Parent, Deb3);
	
	{'EXIT', FromPid, Reason} ->
	    sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
			     ?MODULE, #server_stopped{event='EXIT', reason=Reason, from=FromPid})
	    %unregister_all(?all_registered);
	   ;
	#cask2alloc{client_pid=FromPid} ->
	    ?handler ! #allocate_resource{server=?server, from_pid=FromPid},
	    await_handler(FromPid),
	    active(Parent, Deb);

	#cask2free{client_pid=FromPid, resource=Resource} ->
	    ?handler ! #free_resource{server=?server, from_pid=FromPid, resource=Resource},
	    await_handler(FromPid),
	    active(Parent, Deb);

	#cask4stats{client_pid=FromPid} ->
	    ?handler ! #server_request_data{server=?server},
	    receive
		#handler_reply_data{data=#data_structure{free=Free, allocated=Allocated}} ->
		    ?ssp ! #request_stats{from_pid=FromPid, free=Free, allocated=Allocated},
		    active(Parent, Deb)
	    end
    end.

stop() ->
    case whereis(?server) of
	undefined ->
	    server_not_running;
	_ ->
	    ?server ! {'EXIT', self(), 'normal'},
	    unregister_all(?all_registered),
	    ok
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

await_handler(FromPid) ->
    receive
	#handler_reply{message=Message} ->
	    FromPid ! #server_reply{message=Message};
	#handler_refused{reason=Reason} ->
	    FromPid ! #server_reply{message=lists:concat([request_not_carried_out, Reason])}
    end.

connect_client(ClientPid, Deb) ->
    ClientPid ! #server_reply{message=client_connected},
    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
			    ?MODULE, {out,  #server_reply{message=client_connected}, ClientPid}),
    link(ClientPid),
    Deb2.

write_debug(Dev, Event, Name) ->
    io:format(Dev, "~p: event = ~p~n", [Name, Event]).

system_continue(Parent, Deb, _State) ->
    active(Parent, Deb).

system_terminate(Reason, _Parent, _Deb, _State) ->
    unregister_all(?all_registered),
    exit(Reason).

system_get_state(State) ->
    {ok, State}.

system_replace_state(StateFun, State) ->
    NState = StateFun(State),
    {ok, NState, NState}.
