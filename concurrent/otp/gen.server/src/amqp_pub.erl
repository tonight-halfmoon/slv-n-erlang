-module(amqp_pub).

-export([spawn_link/0, connect/0, pub/1]).

-include("amqp_config.hrl").

-include_lib("amqp_client/include/amqp_client.hrl").

spawn_link() ->
    spawn_link(?MODULE, connect, []).

connect() ->
    %% Connecting to a Broker
    {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
    io:format("Publisher ~p connected to rabbit conn ~p~n.", [self(), Connection]),
    register(?conn, Connection),
    %% A new Channel
    {ok, Channel} = amqp_connection:open_channel(Connection),
    %% A new Queue
    %% 1. Declare an exchange
    Exchange_declare = #'exchange.declare'{exchange = ?exch},
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, Exchange_declare),
    %% 2. Make up the new Queue
    Queue_declare = #'queue.declare'{queue = ?queue},
    #'queue.declare_ok'{queue = ?queue} = amqp_channel:call(Channel, Queue_declare),
    %% Routing Rule
    %Binding = #'queue.bind'{queue = Queue, exchange = Exchange, routing_key = Routing_key},
    %#'queue.bind_ok'{} = amqp_channel:call(Channel, Binding),
    %% Ready to send messages
    io:format("registering the new channel ~p as ~p~n", [Channel, ?ch]),
    register(?ch, Channel),
    process_flag(trap_exit, true),
    active().

pub(Payload) ->
    Publish = #'basic.publish'{exchange = ?exch, routing_key = ?queue},
    Props = #'P_basic'{delivery_mode = 2},
    amqp_channel:cast(whereis(?ch), Publish, #amqp_msg{props = Props, payload = Payload}),
    pubd.

close_ch() ->
    amqp_channel:close(whereis(?ch)).

close_conn() ->
    amqp_connection:close(whereis(?conn)).

active() ->
    receive
	#pub{payload=Payload} ->
	    pub(Payload),
	    active();
	{'EXIT', _From, _Reason} ->
	    close_ch(),
	    close_conn(),
	    terminated
    end.
