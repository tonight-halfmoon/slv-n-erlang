-module(amqp_pub).

-export([start_link/1, send/1]).

-export([init/2, system_continue/3, system_terminate/4,
	 write_debug/3,
	 system_get_state/1, system_replace_state/2]).

-include("amqp_connect.hrl").

-include_lib("amqp_client/include/amqp_client.hrl").

-record(state, {exch, queue}).
-record(send_amqp_msg, {payload, from}).
-define(amqp_pub_proc, genrs_amqp_pub_process).

%% ====================================================================
%% API
%% ====================================================================

start_link(Args) ->
    proc_lib:start_link(?MODULE, init, [self(), Args]).

send(Payload) ->
    io:format("Interface function 'send/1' was called by ~p~n", [self()]),
    ?amqp_pub_proc ! #send_amqp_msg{payload = Payload, from = self()}.

%%%===================================================================
%%% proc_lib callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initialises the process
%%--------------------------------------------------------------------
init(Parent, #amqp_connect_args{exch = Exch, queue = Q}) ->
    register(?amqp_pub_proc, self()),
    Deb = sys:debug_options([statistics, trace]),
    proc_lib:init_ack(Parent, {ok, self()}),
    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3, ?MODULE, {"AMQP Publisher client process is ready"}),
    process_flag(trap_exit, true),
    active(#state{exch = Exch, queue = Q}, Parent, Deb2).

%%%===================================================================
%%% system and debug callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
write_debug(Dev, Event, Name) ->
    io:format(Dev, "~p: event = ~p~n", [Name, Event]).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
system_continue(Parent, Deb, State) ->
    active(State, Parent, Deb).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
system_terminate(Reason, _Parent, Deb, #state{exch=_Exch, queue=_Q}) ->
    sys:handle_debug(Deb, fun ?MODULE:write_debug/3, ?MODULE, {shutdown, Reason}),
    unregister(whereis(?amqp_pub_proc)),
    exit(Reason).

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
system_get_state(State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
system_replace_state(StateFun, State) ->
    NState = StateFun(State),
    {ok, NState, NState}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
active(#state{exch = Exch, queue = Q} = State, Parent, Deb) ->
    receive
	{'EXIT', Parent, Reason} ->
	    unregister(whereis(?amqp_pub_proc)),
	    exit(Reason);
	{system, From, Request} ->
	    sys:handle_system_msg(Request, From, Parent, ?MODULE, Deb, State);
	#send_amqp_msg{payload = Payload, from = From} ->
	    case send_internal(Payload, State, Deb) of
		{error, E, Deb2} ->
		    From ! {error, E},
		    active(State, Parent, Deb2);
		{ok, Deb2} ->
		    From ! {ok, msg_has_been_published, Exch, Q},
		    active(State, Parent, Deb2)
	    end
    end.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
send_internal(Payload, #state{exch = Exch, queue = Q} = State, Deb) ->
     case open_channel(Deb) of
	 {error, E, Deb2} ->
	     {error, E, Deb2};
	 {Connection, Channel, Deb2} ->
	     Publish = #'basic.publish'{exchange = Exch, routing_key = Q},
	     Props = #'P_basic'{delivery_mode = 2},
	     amqp_channel:cast(Channel, Publish, #amqp_msg{props = Props, payload = term_to_binary(Payload)}),
	     Deb3 = sys:handle_debug(Deb2, fun ?MODULE:write_debug/3, ?MODULE, {amq_msg_has_been_sent_to_the_queue}),
	     amqp_channel:close(Channel),
	     amqp_connection:close(Connection),
	     {ok, Deb3}
     end.
