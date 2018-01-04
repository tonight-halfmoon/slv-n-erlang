-module(grs_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Args) ->
    supervisor:start_link(?MODULE, Args).

init(Args) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    GrsChildSpecs = #{id => grs3,
		   start => {grs, start_link, Args},
		   restart => permanent,
		   shutdown => brutal_kill,
		   type => worker,
		   modules => []
		   },
    {ok, {SupFlags, [GrsChildSpecs]}}.
