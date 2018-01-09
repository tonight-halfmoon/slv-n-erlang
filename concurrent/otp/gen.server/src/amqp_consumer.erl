%%%-------------------------------------------------------------------
%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% On the 7th of January 2018
%%%-------------------------------------------------------------------
-module(amqp_consumer).

-export([start_link/0, start_link/1]).

-export([subscribe/2, cask_msg/0]).

-export([system_continue/3, system_terminate/4,
	 write_debug/3,
	 system_get_state/1, system_replace_state/2]).

-include("amqp_connect.hrl").

-include_lib("amqp_client/include/amqp_client.hrl").

-record(state, {ch_pid, received}).

start_link(Args) ->
    {ok, Pid} = proc_lib:start_link(?MODULE, subscribe, [self(), Args]),
    register(?amqp_consumer, Pid),
    Pid.

start_link() ->
    start_link(#amqp_connect{exch=?exch, queue=?queue, ch=?ch, conn=?conn}).

subscribe(Parent, #amqp_connect{exch=Exch, queue=Q, ch=Ch_proc_name, conn=_Conn_proc_name}) ->
    Channel = whereis(Ch_proc_name),
    Deb = sys:debug_options([statistics, trace]),
    Binding = #'queue.bind'{queue = Q, exchange = Exch, routing_key = Q},
    #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding),
    Sub = #'basic.consume'{queue = Q},
    #'basic.consume_ok'{consumer_tag = Tag} = amqp_channel:subscribe(Channel, Sub, self()),
    io:format("Tag: ~p~n", [Tag]),
    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
			    ?MODULE, {"AMQP Client binding for Consumer has been made with tag", Tag}),
    proc_lib:init_ack(Parent, {ok, self()}),
    process_flag(trap_exit, true),
    active(#state{ch_pid=Channel, received=nothing}, Parent, Deb2).

active(#state{ch_pid=Channel, received=LastMsg} = State, Parent, Deb) ->
    receive
	{system, From, Request} ->
	    sys:handle_system_msg(Request, From, Parent, ?MODULE, Deb, State);
	{'EXIT', From, Reason} ->
	    terminate('EXIT', Deb, State),
	    sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
			     ?MODULE, #ampq_connect_stopped{event='EXIT', reason=Reason, from=From});
	#'basic.consume_ok'{} ->
	    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
				    ?MODULE, {received_ok}),
	    active(State, Parent, Deb2);
	#'basic.cancel_ok'{} ->
	    sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
			     ?MODULE, {received_cancel}),
	    terminate(cancel, Deb, State),
	    ok;
	{#'basic.deliver'{delivery_tag = Tag}, {amqp_msg,_, Payload} =_Content} ->
	    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
				    ?MODULE, {received_msg_with_payload, Payload}),
	    amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),
	    io:format("Ack has been sent to the channel~n", []),
	    Deb3 = sys:handle_debug(Deb2, fun ?MODULE:write_debug/3,
				    ?MODULE, {sent_ack}),
	    New_state = #state{ch_pid=Channel, received=Payload},
	    active(New_state, Parent, Deb3);
	#cask_consumer_msg{from=From} ->
	    From ! State,
	    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
				    ?MODULE, {received_cask_consume_msg}),
	    active(State, Parent, Deb2)
    end.
   
cask_msg() ->
    ?amqp_consumer ! #cask_consumer_msg{from=self()},
    receive
	#state{ch_pid=_Channel, received=LastMsg} ->
	    LastMsg
    end.

terminate(Reason, Deb, #state{ch_pid=_Channel, received=_LastMsg}) ->
    unregister(whereis(?amqp_consumer)),
    sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
		     ?MODULE, {terminate, Reason}),
    ok.

write_debug(Dev, Event, Name) ->
    io:format(Dev, "~p: event = ~p~n", [Name, Event]).

system_continue(Parent, Deb, State) ->
    active(State, Parent, Deb).

system_terminate(Reason, _Parent, _Deb, #state{ch_pid=_Channel, received=_LastMsg}) ->
    unregister(whereis(?amqp_consumer)),
    io:format("~p Shutdown because of ~p~n", [?MODULE, Reason]),
    exit(Reason).

system_get_state(State) ->
    {ok, State}.

system_replace_state(StateFun, State) ->
    NState = StateFun(State),
    {ok, NState, NState}.
