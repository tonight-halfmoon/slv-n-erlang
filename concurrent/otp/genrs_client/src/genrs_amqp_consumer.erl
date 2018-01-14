-module(genrs_amqp_consumer).

-export([start_link/0, start_link/1, request_genrs/0]).

-export([init/2, system_continue/3, system_terminate/4, 
	 write_debug/3,
	 system_get_state/1, system_replace_state/2]).

-include_lib("amqp_client/include/amqp_client.hrl").

-include_lib("genrs/include/amqp_connect.hrl").

-define(gcp, genrs_amqp_consumer_proc).

-record(state, {identity, amqp_connect_args, ch_pid, conn_pid, received}).
-record(subscribe, {from}).

start_link() ->
    start_link(#amqp_connect_args{exch=?exch, queue=?queue}).

start_link(Args) ->
    proc_lib:start_link(?MODULE, init, [self(), Args]).

request_genrs() ->
    true.

init(Parent, AMQPConnectArgs) ->
    register(?gcp, self()),
    Deb = sys:debug_options([statistics, trace]),  
    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3, ?MODULE, {"GenRS AMQP Consumer started"}),
    process_flag(trap_exit, true),
    case exchange_declare(Deb2) of
	{error, Deb3} ->
	    proc_lib:init_ack(Parent, {ok, self()}),
	    active(#state{identity = {?gcp, self()}, amqp_connect_args = AMQPConnectArgs, ch_pid = {}, conn_pid= {}, received = {}}, Parent, Deb3);
	{ok, Deb3} ->
    case queue_declare(Deb2) of
	{error, Deb3} ->
	    proc_lib:init_ack(Parent, {ok, self()}),
	    active(#state{identity = {?gcp, self()}, amqp_connect_args = AMQPConnectArgs, ch_pid = {}, conn_pid= {}, received = {}}, Parent, Deb3);
	{ok, Deb3} ->
	    case subscribe(AMQPConnectArgs, Parent, Deb3) of
		{ok, Connection, Channel, Deb4} ->
		    proc_lib:init_ack(Parent, {ok, self()}),
		    active(#state{identity = {?gcp, self()}, amqp_connect_args = AMQPConnectArgs, ch_pid = Channel, conn_pid= Connection, received = {}}, Parent, Deb4);
		{error, _E, Deb4} ->
		    proc_lib:init_ack(Parent, {ok, self()}),
		    active(#state{identity = {?gcp, self()}, amqp_connect_args = AMQPConnectArgs, ch_pid = {}, conn_pid= {}, received = {}}, Parent, Deb4)
	    end
    end
    end.

exchange_declare(Deb) ->
    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3, ?MODULE, {"Let's declare an exchange", {?gcp, self()}}),
    amqp_sp_exchange:declare(),
    receive
	{ok, ExchangeDeclare} ->
	    Deb3 = sys:handle_debug(Deb2, fun ?MODULE:write_debug/3, ?MODULE, {"An exchange was declared as", ExchangeDeclare}),
	    {ok, Deb3};
	{error, Reason} ->
	    Deb3 = sys:handle_debug(Deb2, fun ?MODULE:write_debug/3, ?MODULE, {"Failed to declare an exchange", Reason}),
	    {error, Deb3}
	end.

queue_declare(Deb) ->
    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3, ?MODULE, {"Let's declare a queue", {?gcp, self()}}),
    amqp_sp_queue:declare(),
    receive
	{ok, QueueDeclare} ->
	    Deb3 = sys:handle_debug(Deb2, fun ?MODULE:write_debug/3, ?MODULE, {"A queue was declared as", QueueDeclare}),
	    {ok, Deb3};
	{error, Reason} ->
	    Deb3 = sys:handle_debug(Deb2, fun ?MODULE:write_debug/3, ?MODULE, {"Failed to declare a queue", Reason}),
	    {error, Deb3}
    end.

subscribe(#amqp_connect_args{exch=Exch, queue=Q} = AMQPConnectArgs, Parent, Deb) ->
    case open_channel(Deb) of
	{error, E, Deb2} ->
	    {error, E, Deb2};
	{Connection, Channel, Deb2} ->
	    Binding = #'queue.bind'{queue = Q, exchange = Exch, routing_key = Q},
	    case amqp_channel:call(Channel, Binding) of
		#'queue.bind_ok'{} ->
		    Sub = #'basic.consume'{queue = Q},
		    case amqp_channel:subscribe(Channel, Sub, self()) of
			#'basic.consume_ok'{consumer_tag = Tag} ->
			    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3, ?MODULE, {"GenRS AMQP Consumer has successfully subscribed to the queue with tag", Tag}),
			    {ok, Connection, Channel, Deb2};
			E ->
			    {error, E, Deb}
		    end;
		E ->
		    {error, E, Deb}
	    end
    end.

active(#state{identity=Id, amqp_connect_args = AMQPConnectArgs, ch_pid=Channel, conn_pid=Connection, received=LastMsg} = State, Parent, Deb) ->
    receive
	{'EXIT', Parent, Reason} ->
	    amqp_channel:close(Channel),
	    amqp_connection:close(Connection),
	    unregister(?gcp),
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
	    New_state = #state{identity=Id, amqp_connect_args = AMQPConnectArgs, ch_pid=Channel, conn_pid=Connection, received=binary_to_term(Payload)},
	    active(New_state, Parent, Deb3);
	#subscribe{from = _From} ->
	    % if not has been subscribed
	    active(State, Parent, Deb)
    end.

open_channel(Deb) ->
    case amqp_connection:start(#amqp_params_network{}) of
	{ok, Connection} ->
	    case amqp_connection:open_channel(Connection) of
		{ok, Channel} ->
		    {Connection, Channel, Deb};
		E ->
		    {error, E, Deb}
	    end;
	E ->
	    {error, E, Deb}
    end.

write_debug(Dev, Event, Name) ->
    io:format(Dev, "~p: event = ~p~n", [Name, Event]).

system_continue(Parent, Deb, State) ->
    active(State, Parent, Deb).

system_terminate(Reason, _Parent, Deb, #state{ch_pid=Channel, conn_pid=Connection, received=_LastMsg}) ->
    sys:handle_debug(Deb, fun ?MODULE:write_debug/3, ?MODULE, {shutdown, Reason}),
    amqp_channel:close(Channel),
    amqp_connection:close(Connection),
    unregister(?gcp),
    exit(Reason).

system_get_state(State) ->
    {ok, State}.

system_replace_state(StateFun, State) ->
    NState = StateFun(State),
    {ok, NState, NState}.
