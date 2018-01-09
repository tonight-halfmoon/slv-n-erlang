-module(amqp_pub).

-export([start_link/0, send/1]).

-export([init/2, system_continue/3, system_terminate/4,
	 write_debug/3,
	 system_get_state/1, system_replace_state/2]).

-include("amqp_connect.hrl").

-include_lib("amqp_client/include/amqp_client.hrl").

-record(state, {ch_pid, conn_pid}).

start_link() ->
    proc_lib:start_link(?MODULE, init, [self(), []]).

%  #amqp_connect{exch=?exch, queue=?queue, ch=?ch, conn=?conn}
init(Parent, _Args) ->
    Deb = sys:debug_options([statistics, trace]),
    %% Connecting to a Broker
    {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
    %% A new Channel
    {ok, Channel} = amqp_connection:open_channel(Connection),
    register(?ch, Channel),
    %% A new Queue
    %% 1. Declare an exchange
    Exchange_declare = #'exchange.declare'{exchange = ?exch},
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, Exchange_declare),
    %% 2. Make up the new Queue
    Queue_declare = #'queue.declare'{queue = ?queue},
    #'queue.declare_ok'{queue = ?queue} = amqp_channel:call(Channel, Queue_declare),
    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
			    ?MODULE, "AMQP Client connection for Publisher has been is established~n"),
    proc_lib:init_ack(Parent, {ok, self()}),
    process_flag(trap_exit, true),
    active(Parent, Deb, #state{ch_pid=Channel, conn_pid=Connection}).  

active(Parent, Deb, State) ->
    receive
	{system, From, Request} ->
	    sys:handle_system_msg(Request, From, Parent, ?MODULE, Deb, State);
	{'EXIT', From, Reason} ->
	    terminate('EXIT', Deb, State),
	    sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
			     ?MODULE, #ampq_connect_stopped{event='EXIT', reason=Reason, from=From})
	end.

% #amqp_send{exch=Exch, queue=Q, ch_proc_name=Ch_proc_name, payload=Payload}
send(Payload) ->
    Publish = #'basic.publish'{exchange = ?exch, routing_key = ?queue},
    Props = #'P_basic'{delivery_mode = 2},
    amqp_channel:cast(whereis(?ch), Publish, #amqp_msg{props = Props, payload = Payload}).

terminate(Reason, Deb, #state{ch_pid=Channel, conn_pid=Connection}) ->
    unregister(whereis(?amqp_consumer)),
    unregister(Channel),
    unregister(Connection),
    amqp_channel:close(Channel),
    amqp_connection:close(Connection),
    sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
		     ?MODULE, {terminate, Reason}),
    ok.

write_debug(Dev, Event, Name) ->
    io:format(Dev, "~p: event = ~p~n", [Name, Event]).

system_continue(Parent, Deb, State) ->
    active(State, Parent, Deb).

system_terminate(Reason, _Parent, Deb, #state{ch_pid=Channel, conn_pid=Connection}) ->
    unregister(Channel),
    unregister(Connection),
    amqp_channel:close(Channel),
    amqp_connection:close(Connection),
    io:format("~p Shutdown because of ~p~n", [?MODULE, Reason]),
    exit(Reason).

system_get_state(State) ->
    {ok, State}.

system_replace_state(StateFun, State) ->
    NState = StateFun(State),
    {ok, NState, NState}.
