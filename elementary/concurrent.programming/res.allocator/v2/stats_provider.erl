-module(stats_provider).
-export([mk_stats/0]).
-include("resa_interface.hrl").
-include("resa_intercommunicate.hrl").

mk_stats() ->
    receive
	#request_stats{from_pid=FromPid, free=Free, allocated=Allocated} ->
	    FromPid ! #stats_reply{stats_free=#stats{name=free, length=length(Free)}, stats_allocated=#stats{name=allocated, length=length(Allocated)}}
    end,
    mk_stats().
