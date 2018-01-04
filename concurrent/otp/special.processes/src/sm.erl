%%%-------------------------------------------------------------------
%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2017, 
%%% @doc
%%%
%%% @end
%%% On the 15th of December 2017
%%%-------------------------------------------------------------------
-module(sm).
-export([start_link/0, stop/0]).
-export([init/1]).
-export([system_continue/3, system_terminate/4,
	write_debug/3,
	system_get_state/1, system_replace_state/2]).
-import(sp, [init_sp/1]).
-import(name_lib, [unregister_all/1]).
-define(all_registered, [?sm, ?ssp]).
-include("sm.hrl").
-include("config.hrl").

start_link() ->
    case whereis(?sm) of
	undefined ->
	    proc_lib:start_link(?MODULE, init, [self()]);
	_ ->
	    {sm_running, whereis(?sm)}
    end.

init(Parent) ->
    unregister_all(?all_registered),
    proc_lib:init_ack(Parent, {ok, self()}),
    register(?sm, self()),
    register(?ssp, proc_lib:spawn_link(sp, init_sp, [self()])),
    Deb = sys:debug_options([statistics, trace]),
    State = [?ssp],
    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
			   ?MODULE, #sm_started{pid=self(), name=?sm, state=State}),
    process_flag(trap_exit, true),
    active(State, Parent, Deb2).

active(State, Parent, Deb) ->
    receive

	{system, From, Request} ->
	    sys:handle_system_msg(Request, From, Parent, ?MODULE, Deb, State);

	{'EXIT', FromPid, Reason} ->
	    sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
			    ?MODULE, #sm_stopped{event='EXIT', reason=Reason, from=FromPid}),
	    unregister_all(State);

	{stop, Reason, From} ->
	    sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
			    ?MODULE, #sm_stopped{event=stop, reason=Reason, from=From}),
	    unregister_all(State),
	    exit(Reason);
	
	{register_provider, provider_name=Name, module=Module_name, initfun=Init_fun} ->
	    case proc_lib:start_link(Module_name, Init_fun, [self()]) of
		{ok, Np_pid} ->
		    register(Name, Np_pid),
		    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
					    ?MODULE, #sm_register_provider{provider_name=Name, pid=Np_pid}),
		    State2 = [Name|State],
		    Deb3 = sys:handle_debug(Deb2, fun ?MODULE:write_debug/3,
					    ?MODULE, #sm_state{pid=self(), name=?sm, state=State2}),
		    active(State2, Parent, Deb3)
	    end;

	Msg ->
	    sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
			     ?MODULE, #sm_received_msg{msg=Msg}),
	    unregister_all(State),
	    exit(normal)
    end.

write_debug(Dev, Event, Name) ->
    io:format(Dev, "~p: event = ~p~n", [Name, Event]).

system_continue(Parent, Deb, State) ->
    active(State, Parent, Deb).

system_terminate(Reason, _Paret, _Deb, State) ->
    unregister_all(State),
    exit(Reason).

system_get_state(State) ->
    {ok, State}.

system_replace_state(StateFun, State) ->
    NState = StateFun(State),
    {ok, NState, NState}.

stop() ->
    case whereis(?sm) of
	undefined ->
	    sm_not_running;
	_ ->
	    ?sm ! {'EXIT', self(), 'normal'},
	    ok
    end.
