-module(uniq).
-behaviour(gen_server).

%%API
-export([start_link/0]).
-export([get_id/0]).
%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {count}).

get_id() ->
    {id, ID} = gen_server:call(?MODULE, {}),
    ID.

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
