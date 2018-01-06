-module(simuc).

-export([attempt_alloc/0, attempt_dstats/0]).

-include_lib("eunit/include/eunit.hrl").

attempt_alloc() ->
    genrs:alloc(),
    receive
	Msg ->
	    io:format("~p [~p] received ~p~n", [?MODULE, self(), Msg])
    end.

%%% Asynchronous
attempt_dstats() ->
    spawn(genrs, cask_stats, []).
