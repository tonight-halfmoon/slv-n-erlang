%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% On the 7th of January 2018

-module(amqp_connect).

-export([start_link/1, init/2]).

-export([system_continue/3, system_terminate/4,
	 write_debug/3,
	 system_get_state/1, system_replace_state/2]).

-include_lib("amqp_client/include/amqp_client.hrl").

-include("amqp_connect.hrl").

-record(state, {ch_pid, conn_pid}).

start_link(Args) ->
    proc_lib:start_link(?MODULE, init, [self(), Args]).

init(Parent, _Args) ->
    Deb = sys:debug_options([statistics, trace]),
    %% Connecting to a Broker
    {ok, Connection} = amqp_connection:start(#amqp_params_network{}),
    %% A new Channel
    {ok, Channel} = amqp_connection:open_channel(Connection),
    %% A new Queue
    %% 1. Declare an exchange
    Exchange_declare = #'exchange.declare'{exchange = ?exch},
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, Exchange_declare),
    %% 2. Make up the new Queue
    Queue_declare = #'queue.declare'{queue = ?queue},
    #'queue.declare_ok'{queue = ?queue} = amqp_channel:call(Channel, Queue_declare),
    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
			    ?MODULE, "AMQP Client connection has been is established.~n"),
    proc_lib:init_ack(Parent, {ok, self()}),
    process_flag(trap_exit, true),
    active({ok, #state{ch_pid=Channel, conn_pid=Connection}}, Parent, Deb2).

active(State, Parent, Deb) ->
    receive
	{system, From, Request} ->
	  
	    sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
			     ?MODULE, #ampq_connect_stopped{event=system, reason=no_reason, from=From}),
	    sys:handle_system_msg(Request, From, Parent, ?MODULE, Deb, State);
	{'EXIT', From, Reason} ->
	    terminate('EXIT', State),
	    sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
			     ?MODULE, #ampq_connect_stopped{event='EXIT', reason=Reason, from=From})
    end.

terminate(Reason, #state{ch_pid=Channel, conn_pid=Connection}) ->
    amqp_channel:close(Channel),
    amqp_connection:close(Connection),
    io:format("~p Shutdown because of ~p~n", [?MODULE, Reason]),
    ok.

write_debug(Dev, Event, Name) ->
    io:format(Dev, "~p: event = ~p~n", [Name, Event]).

system_continue(Parent, Deb, State) ->
    active(State, Parent, Deb).

system_terminate(Reason, _Parent, _Deb, #state{ch_pid=Channel, conn_pid=Connection}) ->
    amqp_channel:close(Channel),
    amqp_connection:close(Connection),
    io:format("~p Shutdown because of ~p~n", [?MODULE, Reason]),
    exit(Reason).

system_get_state(State) ->
    {ok, State}.

system_replace_state(StateFun, State) ->
    NState = StateFun(State),
    {ok, NState, NState}.
