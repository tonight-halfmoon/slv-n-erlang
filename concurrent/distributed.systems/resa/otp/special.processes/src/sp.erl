-module(sp).
-export([init_sp/1]).
-export([system_continue/3, system_terminate/4,
	 write_debug/3,
	system_get_state/1]).
-include("sp.hrl").
-include("telecommunication.hrl").
-include("config.hrl").

init_sp(Parent) ->
    Deb = sys:debug_options([statistics, trace]),
    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
			   ?MODULE, #sp_started{pid=self(), name=?ssp, parent=Parent, state={}}),
    process_flag(trap_exit, true),
    active({}, Parent, Deb2).

active(State, Parent, Deb) ->
    receive

	{system, From, Request} ->
	    sys:handle_system_msg(Request, From, Parent, ?MODULE, Deb, State);

	_M = #request_stats{from_pid=FromPid, free=Free, allocated=Allocated} ->
	    sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
			     ?MODULE, #sp_processing{data={Free, Allocated}}),
	   
	    State2 = {{free, length(Free)}, {allocated, length(Allocated)}},
	    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
				   ?MODULE, State2),
	    FromPid ! #stats_reply{stats_free=#stats{name=free, length=length(Free)}, 
				   stats_allocated=#stats{name=allocated, length=length(Allocated)}},
	    active(State2, Parent, Deb2);

	{stop, Reason, From} ->
	    sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
			     ?MODULE, #sp_stopped{event=stop, reason=Reason, from=From}),
	    exit(Reason)
    end.

write_debug(Dev, Event, Name) ->
    io:format(Dev, "~p: event = ~p~n", [Name, Event]).

system_continue(Parent, Deb, State) ->
    active(State, Parent, Deb).

system_terminate(Reason, _Parent, _Deb, _State) ->
    exit(Reason).

system_get_state(State) ->
    {ok, State}.
