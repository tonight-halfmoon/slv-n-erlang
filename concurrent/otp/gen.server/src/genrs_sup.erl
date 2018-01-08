-module(genrs_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(Args) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    GenRSChildSpecs = #{id => genrs_1st,
		      start => {genrs, start_link, Args},
		      restart => permanent,
		      shutdown => brutal_kill,
		      type => worker,
		      modules => [genrs]
		     },
    Rhchildspecs = #{id => rhcp,
		     start => {rh, start_link, Args},
		     restart => permanent,
		     shutdown => brutal_kill,
		     modules => [rh, dh_lib]
		    },
    SMsupspecs = #{id => service_manager_supervisor,
		   start => {sm_sup, start_link, []},
		   restart => transient,
		   type => supervisor
		  },
    {ok, {SupFlags, [GenRSChildSpecs, Rhchildspecs, SMsupspecs]}}.
