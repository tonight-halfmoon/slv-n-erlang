-module(sm_sup).
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-include("sm.hrl").
-include("sp.hrl").
-include("amqp_connect.hrl").

-define(SUP, sm_sup_proc).

start_link() ->
    supervisor:start_link({local, ?SUP}, ?MODULE, []).

init(_Args) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5}, 
    SMchildspecs = #{id => service_manager,
		     start => {sm, start_link, [#sm_start_args{}]},
		     shutdown => brutal_kill
		    },
    SPchildspecs = #{id => ssp,
		     start => {sp, start_link, [#sp_start_args{}]},
		     shutdown => brutal_kill
		    },
    AMQP_publisher_childspecs = #{id => amqp_publisher_child,
				  start => {amqp_pub, start_link, [#amqp_connect_args{exch=?exch, queue=?queue}]},
				  shutdown => brutal_kill
				 },
    {ok, {SupFlags, [SMchildspecs, SPchildspecs, AMQP_publisher_childspecs]}}.
