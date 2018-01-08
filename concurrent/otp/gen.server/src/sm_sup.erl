-module(sm_sup).
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-include("sm.hrl").
-include("sp.hrl").

-define(SUP, smsupproc_name).

start_link() ->
    supervisor:start_link({local, ?SUP}, ?MODULE, []).

init(_Args) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5}, 
    SMchildspecs = #{id => service_manager,
		     start => {sm, start_link, [#sm_start_args{}]},
		     shutdown => 5000
		    },
    SPchildspecs = #{id => ssp,
		     start => {sp, start_link, [#sp_start_args{}]},
		     shutdown => 5000
		    },
    {ok, {SupFlags, [SMchildspecs, SPchildspecs]}}.
