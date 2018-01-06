-module(sp).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-include("sp.hrl").
-include("telecommunication.hrl").
-include("config.hrl").

-record(state, {val}).

start_link(Args) ->
    gen_server:start_link({local, ?ssp}, ?MODULE, Args, [{debug, [trace, statistics]}]).

init(Args) ->
    process_flag(trap_exit, true),
    {ok, #state{val=Args}}.

handle_cast(Msg, State) ->
    io:format("~p received ~p~n", [?ssp, Msg]),
    {noreply, State}.

handle_call(Request, From, State) ->
    io:format("~p received ~p from ~p~n", [?ssp, Request, From]),
    Reply = ok,
    {reply, Reply, State}.

handle_info({'EXIT', From, Reason}, State) ->
    io:format("~p received 'EXIT' from ~p for ~p~n", [?ssp, From, Reason]),
    {noreply, State};
handle_info(Info, State) ->
    io:format("~p received ~p ~n", [?ssp, Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    io:format("~p Shutdown because of ~p~n", [?ssp, Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
