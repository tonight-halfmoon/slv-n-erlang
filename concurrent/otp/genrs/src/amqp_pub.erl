-module(amqp_pub).

-export([start_link/1, send/1]).

-export([init/2, system_continue/3, system_terminate/4,
	 write_debug/3,
	 system_get_state/1, system_replace_state/2]).

-include("amqp_connect.hrl").

-include_lib("amqp_client/include/amqp_client.hrl").

-record(state, {exch, queue}).
-record(which_exchange, {from}).

start_link(Args) ->
    proc_lib:start_link(?MODULE, init, [self(), Args]).

init(Parent, #amqp_connect_args{exch=Exch, queue=Q}) ->
    register(?amqp_pub_proc, self()),
    Deb = sys:debug_options([statistics, trace]),
    proc_lib:init_ack(Parent, {ok, self()}),
    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
			    ?MODULE, {"AMQP Publisher client process is ready"}),
    process_flag(trap_exit, true),
    active(#state{exch=Exch, queue=Q}, Parent, Deb2).

amqp_connect(Exch, Q, Deb) ->
    {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    Exchange_declare = #'exchange.declare'{exchange = Exch},
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, Exchange_declare),
    Queue_declare = #'queue.declare'{queue = Q},
    #'queue.declare_ok'{queue = Q} = amqp_channel:call(Channel, Queue_declare),
    sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
			    ?MODULE, {"AMQP Publisher's channel has been is established"}),
    {Channel, Connection}.

active(#state{exch=Exch, queue=Q} = State, Parent, Deb) ->
    receive
	{'EXIT', Parent, Reason} ->
	    unregister(whereis(?amqp_pub_proc)),
	    exit(Reason);
	{system, From, Request} ->
	    sys:handle_system_msg(Request, From, Parent, ?MODULE, Deb, State);
	#which_exchange{from=From} ->
	    From ! {State, Deb},
	    active(State, Parent, Deb);
	Msg ->
	    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
				    ?MODULE, Msg),
	    active(State, Parent, Deb2)
    end.

send(Payload) ->
    ?amqp_pub_proc ! #which_exchange{from=self()},
    receive
	{#state{exch=Exch, queue=Q}, Deb} ->
	    {Channel, Connection} = amqp_connect(Exch, Q, Deb),
	    Publish = #'basic.publish'{exchange = Exch, routing_key = Q},
	    Props = #'P_basic'{delivery_mode = 2},
	    amqp_channel:cast(Channel, Publish, #amqp_msg{props = Props, payload = term_to_binary(Payload)}),
	    sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
			     ?MODULE, {amq_msg_has_been_sent_to_the_queue}),
	    amqp_channel:close(Channel),
	    amqp_connection:close(Connection)
    end.

write_debug(Dev, Event, Name) ->
    io:format(Dev, "~p: event = ~p~n", [Name, Event]).

system_continue(Parent, Deb, State) ->
    active(State, Parent, Deb).

system_terminate(Reason, _Parent, Deb, #state{exch=_Exch, queue=_Q}) ->
    sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
		     ?MODULE, {shutdown, Reason}),
    unregister(whereis(?amqp_pub_proc)),
    exit(Reason).

system_get_state(State) ->
    {ok, State}.

system_replace_state(StateFun, State) ->
    NState = StateFun(State),
    {ok, NState, NState}.
