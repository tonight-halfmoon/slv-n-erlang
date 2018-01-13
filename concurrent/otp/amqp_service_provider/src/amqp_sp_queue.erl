-module(amqp_sp_queue).

-export([start_link/1]).

-export([init/2, system_continue/3, system_terminate/4,
	 write_debug/3,
	 system_get_state/1, system_replace_state/2]).

-include_lib("amqp_client/include/amqp_client.hrl").

-record(state, {queue}).

-define(queue_declare_proc, amqp_sp_queue_declare_proc).

start_link(QueueDeclareArgs) ->
    proc_lib:start_link(?MODULE, init, [self(), QueueDeclareArgs]).

init(Parent, QueueDeclareArgs) ->
    Deb = sys:debug_options([statistics, trace]),
    proc_lib:init_ack(Parent, {ok, self()}),
    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3, ?MODULE, {"AMQP Queue Declarer started"}),
    process_flag(trap_exit, true),
    active(#state{queue = QueueDeclareArgs}, Parent, Deb2).

active(State, Parent, Deb) ->
    receive
	_ ->
	    active(State, Parent, Deb)
    end.

write_debug(Dev, Event, Name) ->
    io:format(Dev, "~p: event = ~p~n", [Name, Event]).

system_continue(Parent, Deb, State) ->
    active(State, Parent, Deb).

system_terminate(Reason, _Parent, Deb, _State) ->
    sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
		     ?MODULE, {shutdown, Reason}),
    exit(Reason).

system_get_state(State) ->
    {ok, State}.

system_replace_state(StateFun, State) ->
    NState = StateFun(State),
    {ok, NState, NState}.
