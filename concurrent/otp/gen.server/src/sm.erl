%%%-------------------------------------------------------------------
%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2017, 
%%% @doc
%%%
%%% @end
%%% On the 15th of December 2017
%%%-------------------------------------------------------------------
-module(sm).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(all_registered, [?sm, ?ssp]).

-import(sp, [init_sp/1]).
-import(name_lib, [unregister_all/1]).

-include("sm.hrl").
-include("config.hrl").

start_link(Args) ->
    gen_server:start_link({local, ?sm}, ?MODULE, Args, [{debug, [trace, statistics]}]).

init(Args) ->
    %register(?ssp, proc_lib:spawn_link(sp, init_sp, [self()])),
    process_flag(trap_exit, true),
    {ok, #state_internal{val=Args}}.

handle_cast(Msg, State) ->
    io:format("~p received ~p~n", [?MODULE, Msg]),
    {noreply, State}.

handle_call(Request, From, State) ->
    io:format("~p received ~p from ~p~n", [?MODULE, Request, From]),
    Reply = ok,
    {reply, Reply, State}.

handle_info({'EXIT', From, Reason}, State) ->
    io:format("~p received 'EXIT' from ~p for ~p~n", [?MODULE, From, Reason]),
    {noreply, State};
handle_info(Info, State) ->
    io:format("~p received ~p ~n", [?MODULE, Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    io:format("Shutdown because of ~p~n", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
