-module(resa_server).
-behaviour(application).
-export([start/2, stop/1]).
-export([start_link/1, stop/0]).
-export([init/2]).
-export([system_continue/3, system_terminate/4,
	write_debug/3,
	system_get_state/1, system_replace_state/2]).
-include("config_internal.hrl").
-include("config.hrl").
-include("telecommunication.hrl").
-import(handler, [init_dh/2]).
-import(name_lib, [unregister_all/1]).
-define(all_registered, [?server, ?handler]).

start(_Type, Args) ->
    ?MODULE:start_link(Args).

stop(_State) ->
    ok.

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

    Deb = sys:debug_options([statistics, trace]),
    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
			    ?MODULE, #server_started{pid=self(), name=?server, state=Free}),
    proc_lib:init_ack(Parent, {ok, self()}),
    process_flag(trap_exit, true),
    active(Parent, Deb2).

%%% Assumptions:
%%% Function 'active' assumes that service providers have been started by Service Manager

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
	Msg = #cask2alloc{client_pid=FromPid} ->
	    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
				   ?MODULE, {in, #server_received{from=FromPid, msg=Msg}}),
	    ?handler ! #allocate_resource{server=?server, from_pid=FromPid},
	    Deb3 = await_handler(FromPid, Deb2),
	    active(Parent, Deb3);

	Msg = #cask2free{client_pid=FromPid, resource=Resource} ->
	    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
				   ?MODULE, {in, #server_received{from=FromPid, msg=Msg}}),
	    ?handler ! #free_resource{server=?server, from_pid=FromPid, resource=Resource},
	    Deb3 = await_handler(FromPid, Deb2),
	    active(Parent, Deb3);

	Msg = #cask4stats{client_pid=FromPid} ->
	    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
				   ?MODULE, {in, #server_received{from=FromPid, msg=Msg}}),
	    ?handler ! #server_request_data{server=?server},
	    receive
		#handler_reply_data{data=#data_structure{free=Free, allocated=Allocated}} ->
		    ?ssp ! #request_stats{from_pid=FromPid, free=Free, allocated=Allocated},
		    active(Parent, Deb2);
		Msg ->
		    Deb3 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
					    ?MODULE, {in, #server_received_unexpected{from=?handler, msg=Msg}}),
		    active(Parent, Deb3)
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

await_handler(From_pid, Deb) ->
    receive
	#handler_reply{message=Message} ->
	    From_pid ! M = #server_reply{message=Message},
	    sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
			    ?MODULE, {out, #server_has_sent{msg=M, to=From_pid}});
	#handler_refused{reason=Reason} ->
	    From_pid ! M = #server_reply{message=lists:concat([request_not_carried_out, Reason])},
	    sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
			     ?MODULE, {out, #server_has_sent{msg=M, to=From_pid}})
    end.

connect_client(ClientPid, Deb) ->
    ClientPid ! M = #server_reply{message=client_connected},
    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
			    ?MODULE, {out, M, ClientPid}),
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
