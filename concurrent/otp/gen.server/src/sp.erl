-module(sp).

-behaviour(gen_server).

-export([start_link/1, dstats/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-include("sp.hrl").
-include("config.hrl").

-record(state, {val}).

start_link(Args) ->
    gen_server:start_link({local, ?ssp}, ?MODULE, Args, [{debug, [trace, statistics]}]).

dstats(Request) ->
    self() ! gen_server:call(?ssp, Request).

init(Args) ->
    process_flag(trap_exit, true),
    {ok, #state{val=Args}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(#quickstats_on_dbrief{free=Free, allocated=Allocated}, _From, State) ->
    Reply = #dstats{stats_free=#bse{name=free, length=length(Free)}, 
		    stats_allocated=#bse{name=allocated, length=length(Allocated)}},
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_info({'EXIT', _From, _Reason}, State) ->
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, _State) ->
    io:format("~p Shutdown because of ~p~n", [?ssp, Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
