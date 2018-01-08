%%%-------------------------------------------------------------------
%%% @author Taghrid Elghafari <sunrise@Taghrids-MacBook.local>
%%% @copyright (C) 2017, Taghrid Elghafari
%%% @doc
%%%
%%% @end
%%% On the 24th December 2017
%%%-------------------------------------------------------------------
-module(genrs).

-behaviour(gen_server).

%% API
-export([start_link/1, freeup/1, alloc/0, cask_dstats/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("genrs.hrl").
-include("config.hrl").
-include("rh.hrl").
-include("sp.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Free) ->
    gen_server:start_link({local, ?server}, ?MODULE, Free, [{debug, [trace, statistics]}]).

freeup(Res) ->
    self() ! gen_server:call(?server, #cask2free{resource=Res}).

alloc() ->    
    self() ! gen_server:call(?server, #cask2alloc{}).

cask_dstats() ->
    gen_server:cast(?server, #cask_dstats{}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Args) ->
    process_flag(trap_exit, true),
    {ok, #state{free=Args, allocated=[]}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call(#cask2free{resource=_Res} = Request, _From, State) ->
    rh:freeup(Request),
    receive
	#rh_ok{more=More, new_state= #state{free=_Free, allocated=_Allocated} = New_state} ->
	    Reply = #ok{more=More},
	    {reply, Reply, New_state};
	#rh_error{reason=Reason} ->
	    Reply = #error{reason=Reason},
	    {reply, Reply, State}
    end;
handle_call(#cask2alloc{} = Request, _From, State) ->
    rh:alloc(Request),
    receive
	#rh_ok{more=More, new_state= #state{free=_Free, allocated=_Allocated} = New_state} ->
	    Reply = #ok{more=More},
	    {reply, Reply, New_state};
	#rh_error{reason=Reason} ->
	    Reply = #error{reason=Reason},
	    {reply, Reply, State}
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_cast(#cask_dstats{} = _Request, #state{free=_Free, allocated=_Allocated} = State) ->
    rh:rbrief(#cask_dbrief{}),
    receive
	#rh_dbrief{ds=#data_structure{free=BFree, allocated=BAllocated}} ->
	    sp:dstats(#quickstats_on_dbrief{free=BFree, allocated=BAllocated}),
	    {noreply, State};
	_ ->
	    {noreply, State}
    end;
handle_cast(Msg, State) ->
    io:format("~p received `asynchronous request` ~p~n", [?server, Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, State) ->
    io:format("~p received info 'EXIT' from ~p for ~p~n", [?server, Pid, Reason]),
    {noreply, State};
handle_info(Info, State) ->
    io:format("~p received info ~p ~n", [?server, Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, _State) ->
    io:format("Shutdown because of ~p~n", [Reason]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
