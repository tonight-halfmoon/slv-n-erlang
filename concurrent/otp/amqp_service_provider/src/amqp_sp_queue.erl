-module(amqp_sp_queue).

-export([start_link/1, declare/0]).

-export([init/2, system_continue/3, system_terminate/4,
	 write_debug/3,
	 system_get_state/1, system_replace_state/2]).

-include_lib("amqp_client/include/amqp_client.hrl").

-record(state, {queue_declare_args, queue_declared}).
-record(declare, {queue_declare_args, from}).

-define(queue_declare_proc, amqp_sp_queue_declare_proc).

%% ====================================================================
%% API
%% ====================================================================

start_link(QueueDeclareArgs) ->
    proc_lib:start_link(?MODULE, init, [self(), QueueDeclareArgs]).

declare() ->
    io:format("Interface function 'declare/0' was called by ~p~n", [self()]),
    ?queue_declare_proc ! #declare{queue_declare_args = {}, from = self()}.

%%%===================================================================
%%% proc_lib callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initialises the process
%%--------------------------------------------------------------------
init(Parent, QueueDeclareArgs) ->
    register(?queue_declare_proc, self()),
    Deb = sys:debug_options([statistics, trace]),
    proc_lib:init_ack(Parent, {ok, self()}),
    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3, ?MODULE, {"AMQP Queue Declarer started"}),
    process_flag(trap_exit, true),
    active(#state{queue_declare_args = QueueDeclareArgs, queue_declared = {}}, Parent, Deb2).

%%%===================================================================
%%% system and debug callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
write_debug(Dev, Event, Name) ->
    io:format(Dev, "~p: event = ~p~n", [Name, Event]).

system_continue(Parent, Deb, State) ->
    active(State, Parent, Deb).

system_terminate(Reason, _Parent, Deb, _State) ->
    sys:handle_debug(Deb, fun ?MODULE:write_debug/3, ?MODULE, {shutdown, Reason}),
    unregister(whereis(?queue_declare_proc)),
    exit(Reason).

system_get_state(State) ->
    {ok, State}.

system_replace_state(StateFun, State) ->
    NState = StateFun(State),
    {ok, NState, NState}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
active(#state{queue_declare_args = QueueDeclareArgs, queue_declared = ActualQueueDeclaredArgs} = State, Parent, Deb) ->
    receive
	{system, From, Request} ->
	    sys:handle_system_msg(Request, From, Parent, ?MODULE, Deb, State);
	{'EXIT', Parent, Reason} ->
	    sys:handle_debug(Deb, fun ?MODULE:write_debug/3, ?MODULE, {"Received 'EXIT' from", Parent, "because", Reason}),
	    unregister(whereis(?queue_declare_proc)),
	    exit(Reason);
	#declare{queue_declare_args = _anotherExchangeArgs, from = From} ->
	    case has_been_declared(State) of
		false ->
		    case declare2(QueueDeclareArgs, Deb) of
			{ok, QueueDeclaredAs, Deb2} ->
			    From ! {ok, QueueDeclaredAs},
			    active(#state{queue_declare_args = QueueDeclareArgs, queue_declared = QueueDeclaredAs}, Parent, Deb2);
			{error, E, Deb2} ->
			    From ! {error, E},
			    active(State, Parent, Deb2)
		    end;
		true ->
		    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3, ?MODULE, {"Queue", ActualQueueDeclaredArgs, "has been already declared"}),
		    From ! {error, queue_has_been_already_declared},
		    active(State, Parent, Deb2)
	    end
    end.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
declare2(QueueDeclareArgs, Deb) ->
    case open_channel(Deb) of
	{error, E, Deb2} ->
	    {error, E, Deb2};
	{Connection, Channel, Deb2} ->
	    Queue_declare = #'queue.declare'{queue = QueueDeclareArgs},
	    case amqp_channel:call(Channel, Queue_declare) of
		#'queue.declare_ok'{queue = QueueDeclareArgs} ->
		    Deb3 = sys:handle_debug(Deb2, fun ?MODULE:write_debug/3, ?MODULE, {"Queue has been successfully declared", QueueDeclareArgs, Queue_declare}),
		    amqp_channel:close(Channel),
		    amqp_connection:close(Connection),
		    {ok, Queue_declare, Deb3};
		E ->
		    amqp_channel:close(Channel),
		    amqp_connection:close(Connection),
		    Deb3 = sys:handle_debug(Deb2, fun ?MODULE:write_debug/3, ?MODULE, {"Failed to declare a queue", E}),
		    {error, E, Deb3}
	    end
    end.

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
has_been_declared(#state{queue_declare_args = _QueueDeclareArgs, queue_declared = {}}) ->
    false;
has_been_declared(#state{queue_declare_args = QueueDeclareArgs, queue_declared = #'queue.declare'{ticket = _,
												  queue = QueueDeclareArgs,
												  passive = _,
												  durable = _,
												  exclusive = _,
												  auto_delete = _,
												  nowait = _,
												  arguments = _}}) ->
    true.
