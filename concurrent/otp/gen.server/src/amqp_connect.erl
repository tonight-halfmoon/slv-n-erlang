%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% On the 7th of January 2018

-module(amqp_connect).

-export([spawn_link/1]).

-export([init/1]).

-include_lib("amqp_client/include/amqp_client.hrl").

-include("amqp_config.hrl").

-record(state, {pid, name}).

spawn_link([]) ->
    spawn_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    %% Connecting to a Broker
    {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
    io:format("AMQP Client connection has been is established ~p~n.", [Connection]),
    %register(?conn, Connection),
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
    io:format("registering the new channel ~p as ~p~n", [Channel, ?ch]),
    register(?ch, Channel),
    process_flag(trap_exit, true),
    active({ok, #state{pid=self(), name=?ch}}).

active(State) ->
    receive
	{'EXIT', _From, _Reason} ->
	    terminate('EXIT', State),
	    {noreply, State}
    end.

terminate(Reason, _State) ->
    Ch_pid = whereis(?ch),
    unregister(Ch_pid),
    amqp_channel:close(Ch_pid),
    Conn_pid = whereis(?conn),
    unregister(Conn_pid),
    amqp_connection:close(Conn_pid),
    io:format("~p Shutdown because of ~p~n", [?MODULE, Reason]),
    ok.
