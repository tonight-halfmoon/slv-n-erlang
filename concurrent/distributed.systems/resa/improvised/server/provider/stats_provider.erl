-module(stats_provider).
-export([mk_stats/0]).
-include("../interface_provider.hrl").
-include("../../config/telecommunication.hrl").

%%% Experience
%%% Another method of message flow. Stats provider is free to send back it is transformation result to the client. Since it does not carry anything about the server's data models.
%%% Sure! It is so naiive what implemented here. However, the goal is to apply the concept, learn and enjoy the game.

mk_stats() ->
    receive
	M = #request_stats{from_pid=FromPid, free=Free, allocated=Allocated} ->
	    io:format("Stats provider received message ~p~n", [M]),
	    FromPid ! #stats_reply{stats_free=#stats{name=free, length=length(Free)}, stats_allocated=#stats{name=allocated, length=length(Allocated)}}
    end,
    mk_stats().
