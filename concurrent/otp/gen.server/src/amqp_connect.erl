%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% On the 7th of January 2018

-module(amqp_connect).

-export([spawn_link/1, init/2]).

-export([system_continue/3, system_terminate/4,
	 write_debug/3]).

-include_lib("amqp_client/include/amqp_client.hrl").

-include("amqp_connect.hrl").

-record(state, {pid, name}).

spawn_link([]) ->
    spawn_link({local, ?MODULE}, ?MODULE, init, [self(), #amqp_connect{exch=?exch, queue=?queue, ch=?ch, conn=?conn}]).

init(Parent, _Args) ->
    %% Connecting to a Broker
    {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
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
    Deb = sys:debug_options([statistics, trace]),
    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
			    ?MODULE, "AMQP Client connection has been is established.~nRegistering a new channel on RabbitMQ"),
    register(?ch, Channel),
    proc_lib:init_ack(Parent, {ok, self()}),
    process_flag(trap_exit, true),
    active({ok, #state{pid=self(), name=?ch}}, Parent, Deb2).

active(State, Parent, Deb) ->
    receive
	{system, From, Request} ->
	    sys:handle_system_msg(Request, From, Parent, ?MODULE, Deb, State);
	{'EXIT', _From, _Reason} ->
	    terminate('EXIT', State)
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

write_debug(Dev, Event, Name) ->
    io:format(Dev, "~p: event = ~p~n", [Name, Event]).

system_continue(Parent, Deb, State) ->
    active(State, Parent, Deb).

system_terminate(Reason, _Parent, _Deb, _State) ->
    exit(Reason).
