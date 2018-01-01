%%%-------------------------------------------------------------------
%%% @author Taghrid Elghafari <sunrise@Taghrids-MacBook.local>
%%% @copyright (C) 2017, Taghrid Elghafari
%%% @doc
%%%
%%% @end
%%% On the 27th December 2017
%%%-------------------------------------------------------------------
-module(resa_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, rssp_sup).


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
    supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

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

    Rscspecs = #{id => resas2,
	       start => {resa_server, start_link, Args},
	       shutdown => 5000,
	       modules => [resa_server]
	      },
    SMsupcspecs = #{id => csmsup,
		   start => {sm_sup, start_link, []},
		   restart => transient,
		   type => supervisor},
    {ok, {SupFlags, [Rscspecs, SMsupcspecs]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

