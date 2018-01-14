%%%-------------------------------------------------------------------
%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% On the 7th of January 2018
%%%-------------------------------------------------------------------
-module(amqp_consumer).

-export([start_link/1, cask4_msg/0]).

-export([subscribe/2, system_continue/3, system_terminate/4,
	 write_debug/3,
	 system_get_state/1, system_replace_state/2]).

-include("amqp_connect.hrl").

-include_lib("amqp_client/include/amqp_client.hrl").

-record(state, {ch_pid, conn_pid, received}).

start_link(Args) ->
    proc_lib:start_link(?MODULE, subscribe, [self(), Args]).

cask4_msg() ->
    ?amqp_consumer_proc ! #cask4_consumer_msg{from=self()},
    receive
	LastMsg ->
	    LastMsg
    end.

subscribe(Parent, #amqp_connect_args{exch=Exch, queue=Q}) ->
    register(?amqp_consumer_proc, self()),
    Deb = sys:debug_options([statistics, trace]),
    {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    Binding = #'queue.bind'{queue = Q, exchange = Exch, routing_key = Q},
    #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding),
    Sub = #'basic.consume'{queue = Q},
    #'basic.consume_ok'{consumer_tag = Tag} = amqp_channel:subscribe(Channel, Sub, self()),
    proc_lib:init_ack(Parent, {ok, self()}),
    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
			    ?MODULE, {"AMQP Client Consumer's binding has been made with tag", Tag}),
    process_flag(trap_exit, true),
    active(#state{ch_pid=Channel, received=nothing}, Parent, Deb2).

active(#state{ch_pid=Channel, conn_pid=Connection, received=LastMsg} = State, Parent, Deb) ->
    receive
	{'EXIT', Parent, Reason} ->
	    amqp_channel:close(Channel),
	    amqp_connection:close(Connection),
	    unregister(?amqp_consumer_proc),
	    exit(Reason);
	{system, From, Request} ->
	    sys:handle_system_msg(Request, From, Parent, ?MODULE, Deb, State);
	#'basic.consume_ok'{} ->
	    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3, ?MODULE, {received_ok}),
	    active(State, Parent, Deb2);
	#'basic.cancel_ok'{} ->
	    sys:handle_debug(Deb, fun ?MODULE:write_debug/3, ?MODULE, {received_cancel}),
	    active(State, Parent, Deb);
	{#'basic.deliver'{delivery_tag = Tag}, {amqp_msg, _, Payload} =_Content} ->
	    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3, ?MODULE, {received_msg_with_payload, Payload}),
	    amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),
	    Deb3 = sys:handle_debug(Deb2, fun ?MODULE:write_debug/3, ?MODULE, {sent_ack}),
	    New_state = #state{ch_pid=Channel, conn_pid=Connection, received=binary_to_term(Payload)},
	    active(New_state, Parent, Deb3);
	#cask4_consumer_msg{from=From} ->
	    From ! LastMsg,
	    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3, ?MODULE, {received_cask_consume_msg}),
	    active(State, Parent, Deb2)
    end.

write_debug(Dev, Event, Name) ->
    io:format(Dev, "~p: event = ~p~n", [Name, Event]).

system_continue(Parent, Deb, State) ->
    active(State, Parent, Deb).

system_terminate(Reason, _Parent, Deb, #state{ch_pid=Channel, conn_pid=Connection, received=_LastMsg}) ->
    sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
		     ?MODULE, {shutdown, Reason}),
    amqp_channel:close(Channel),
    amqp_connection:close(Connection),
    unregister(?amqp_consumer_proc),
    exit(Reason).

system_get_state(State) ->
    {ok, State}.

system_replace_state(StateFun, State) ->
    NState = StateFun(State),
    {ok, NState, NState}.
