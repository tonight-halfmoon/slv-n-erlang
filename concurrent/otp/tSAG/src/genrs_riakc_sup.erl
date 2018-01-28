%%%-------------------------------------------------------------------
%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% On the 28th of January 2018
%%%-------------------------------------------------------------------
-module(genrs_riakc_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init(Args) ->
    SupFlags = #{strategy => one_for_one,
		 intensity => 1,
		 period => 5},

    RiakcGeoCheckin = #{id => riakc_geocheckin_child,
	       start => {riakc_geocheckin, start_link, [Args]},
	       restart => permanent,
	       shutdown => 5000,
	       type => worker,
	       modules => []},

    {ok, {SupFlags, [RiakcGeoCheckin]}}.
