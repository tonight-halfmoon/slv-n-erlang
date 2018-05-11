%%%----------------------------------------------------------------
%%% @author name <email@exmaple.com>
%%% [http://webpage.org]
%%% @copyright yyyy Name
%%% @doc This module defines a server process that listens for incoming
%%%      TCP connections and allows the user to execute commands via
%%%      that TCP stream
%%% @end
%%%----------------------------------------------------------------

-module(tr_server).
-behaviour(gen_server).

%% API
-export([
	 start_link/1,
	 start_link/0,
	 get_count/0,
	 stop/0
	]).

%% Gen Server Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 1055).

-record(state, {port, lsock, request_count=0}).


%%%================================================================
%%% API
%%%================================================================

%%-----------------------------------------------------------------
%% @doc
%% Starts the server
%% 
%% @spec start_link(Port::integer()) -> {ok, Pid}
%% where
%% Pid = pid()
%% @end
%%-----------------------------------------------------------------
start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port],[]).

%% @spec start_link() -> {ok, Pid}
%% @equiv start_link(Port::integer())
start_link() ->
    start_link(?DEFAULT_PORT).

%%-----------------------------------------------------------------
%% @doc fetch the number of request mae to this server.
%% @spec get_count() -> {ok, Cound}
%% where
%% Count =integer()
%%
%% Send a synchronous message to a gen_server process
%% @end
%%-----------------------------------------------------------------
get_count() ->
    gen_server:call(?SERVER, get_count).

%%-----------------------------------------------------------------
%% @doc stops the server.
%%% @spec stop() -> ok
%%
%% send an asynchronous messge to a gen_server process
%% @end
%%-----------------------------------------------------------------
stop() ->
    gen_server:cast(?SERVER, stop).

%%-----------------------------------------------------------------
%% Generic Server Callback definitions
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% @doc INIT/1 - Callback for GEN_SERVER:start_link
%% @end
%%-----------------------------------------------------------------
init([Port]) ->
    % We set this socket to active true which tells gen_tcp to send all
    % incoming TCP data to our process as messages in our process mailbox.
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
    {ok, #state{port=Port, lsock=LSock}, 0}.

%% @doc handle_call/3 - Callback for gen_server:call
handle_call(get_count, _From, State) ->
    {reply, {ok, State#state.request_count}, State}.


handle_cast(stop, State) ->
    {stop, ok, State}.

handle_info({tcp, Socket, RawData}, State) ->
    RequestCount = State#state.request_count,
    try
	MFA = re:replace(RawData, "\r\n$", "", [{return, list}]),
	{match, [M, F, A]} = re:run(MFA, "(.*):(.*)\s*\\((.*)\s*\\)\s*.\s*$",
				   [{capture, [1,2,3], list}, ungreedy]),
	Result = apply(list_to_atom(M), list_to_atom(F), args_to_terms(A)),
	gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Result]))
    catch
	_C:E ->
	    gen_tcp:send(Socket, io_lib:fwrite("~p~n", [E]))
    end,
    {noreply, State#state{request_count = RequestCount + 1}};
handle_info(timeout, #state{lsock=LSock} = State) ->
    {ok, _Sock} = gen_tcp:accept(LSock),
    {noreply, State}.


%%%==============================================================
%%% Internal functions
%%%==============================================================

args_to_terms([]) ->
    [];
args_to_terms(RawArgs) ->
    {ok, Toks, _Line} = erl_scan:string("[" ++ RawArgs ++ "]. ", 1),
    {ok, Args} = erl_parse:parse_term(Toks),
    Args.
