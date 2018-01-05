-module(sm_sup).
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SUP, smsup).

start_link() ->
    supervisor:start_link({local, ?SUP}, ?MODULE, []).

init(Args) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5}, 
    Childs = [#{id => smsup, 
		start => {sm, start_link, [Args]}, 
		shutdown => 5000
		}],
    {ok, {SupFlags, Childs}}.
