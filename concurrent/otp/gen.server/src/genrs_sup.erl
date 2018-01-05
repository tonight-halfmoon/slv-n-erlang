-module(genrs_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(Args) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    GrsChildSpecs = #{id => genrs_1st,
		   start => {genrs, start_link, Args},
		   restart => permanent,
		   shutdown => brutal_kill,
		   type => worker,
		   modules => [genrs]
		   },
    SMsupspecs = #{id => csmsup,
		   start => {sm_sup, start_link, []},
		   restart => transient,
		   type => supervisor
		  },
    {ok, {SupFlags, [GrsChildSpecs, SMsupspecs]}}.
