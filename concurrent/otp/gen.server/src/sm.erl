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

-include("sm.hrl").
-include("common.hrl").
-include("config.hrl").

start_link(Args) ->
    gen_server:start_link({local, ?sm}, ?MODULE, Args, [{debug, [trace, statistics]}]).

init(Args) ->
    process_flag(trap_exit, true),
    {ok, #state_internal{val=Args}}.

handle_cast(Msg, State) ->
    io:format("~p received ~p~n", [?sm, Msg]),
    {noreply, State}.

handle_call(Request, From, State) ->
    io:format("~p received ~p from ~p~n", [?sm, Request, From]),
    Reply = ok,
    {reply, Reply, State}.

handle_info({'EXIT', From, Reason}, State) ->
    io:format("~p received 'EXIT' from ~p for ~p~n", [?sm, From, Reason]),
    {noreply, State};
handle_info(Info, State) ->
    io:format("~p received ~p ~n", [?sm, Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    io:format("~p Shutdown because of ~p~n", [?sm, Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
