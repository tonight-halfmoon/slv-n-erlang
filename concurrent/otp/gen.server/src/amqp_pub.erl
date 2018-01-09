-module(amqp_pub).

-export([start_link/1, send/1]).

-export([init/2, system_continue/3, system_terminate/4,
	 write_debug/3,
	 system_get_state/1, system_replace_state/2]).

-include("amqp_connect.hrl").

-include_lib("amqp_client/include/amqp_client.hrl").

-record(state, {ch_pid, conn_pid, exch, queue}).
-record(which_exchange, {from}).
start_link(Args) ->
    proc_lib:start_link(?MODULE, init, [self(), Args]).

init(Parent, #amqp_connect_args{exch=Exch, queue=Q}) ->
    register(?amqp_pub_proc, self()),
    Deb = sys:debug_options([statistics, trace]),
    %% Connecting to a Broker
    {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
    %% A new Channel
    {ok, Channel} = amqp_connection:open_channel(Connection),
    %% A new Queue
    %% 1. Declare an exchange
    Exchange_declare = #'exchange.declare'{exchange = Exch},
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, Exchange_declare),
    %% 2. Make up the new Queue
    Queue_declare = #'queue.declare'{queue = Q},
    #'queue.declare_ok'{queue = Q} = amqp_channel:call(Channel, Queue_declare),
    proc_lib:init_ack(Parent, {ok, self()}),    
    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
			    ?MODULE, {"AMQP Client connection for Publisher has been is established"}),
    process_flag(trap_exit, true),
    active(Parent, Deb2, #state{ch_pid=Channel, conn_pid=Connection, exch=Exch, queue=Q}).

active(Parent, Deb, #state{ch_pid=Channel, conn_pid=Connection, exch=Exch, queue=Q} = State) ->
    receive
	{system, From, Request} ->
	    sys:handle_system_msg(Request, From, Parent, ?MODULE, Deb, State);
	{'EXIT', From, Reason} ->
	    terminate('EXIT', Deb, State),
	    sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
			     ?MODULE, {event,'EXIT', reason, Reason, from, From});
	#which_exchange{from=From} ->
	    From ! {Channel, Exch, Q},
	    active(Parent, Deb, State);
	Msg ->
	    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
				    ?MODULE, Msg),
	    active(Parent, Deb2, State)
    end.

send(Payload) ->
    ?amqp_pub_proc ! #which_exchange{from=self()},
    receive
	{Channel, Exch, Q} ->
	    Publish = #'basic.publish'{exchange = Exch, routing_key = Q},
	    Props = #'P_basic'{delivery_mode = 2},
	    amqp_channel:cast(Channel, Publish, #amqp_msg{props = Props, payload = term_to_binary(Payload)})
    end.

terminate(Reason, Deb, #state{ch_pid=Channel, conn_pid=Connection, exch=_Exch, queue=_Q}) ->
    amqp_channel:close(Channel),
    amqp_connection:close(Connection),
    unregister(whereis(?amqp_pub_proc)),
    sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
		     ?MODULE, {terminated, Reason}),
    ok.

write_debug(Dev, Event, Name) ->
    io:format(Dev, "~p: event = ~p~n", [Name, Event]).

system_continue(Parent, Deb, State) ->
    active(State, Parent, Deb).

system_terminate(Reason, _Parent, _Deb, #state{ch_pid=Channel, conn_pid=Connection, exch=_Exch, queue=_Q}) ->
    io:format("~p Shutdown because of ~p~n", [?MODULE, Reason]),
    amqp_channel:close(Channel),
    amqp_connection:close(Connection),
    unregister(whereis(?amqp_pub_proc)),
    exit(Reason).

system_get_state(State) ->
    {ok, State}.

system_replace_state(StateFun, State) ->
    NState = StateFun(State),
    {ok, NState, NState}.
