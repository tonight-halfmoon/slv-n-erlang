-module(ait).
-export([start/0, stop/0]).

-export([after_each/1]).
-include_lib("eunit/include/eunit.hrl").

-include("../src/sm.hrl").
-include("../src/sp.hrl").
-include("../src/rh.hrl").
-include("../include/config.hrl").

start_test_() ->
    {
      "When GenRS is loaded and started, then the supervision tree must be built, and the following process must have been started: `genrs_server`, `service_manager`, `service_stats_provider` and `rh`",
      {setup, 
       fun() -> ?MODULE:start() end,
       fun ?MODULE:after_each/1,
       fun(Actual) ->
	       [
		?_assertEqual(ok, Actual),
		?_assertMatch(P when true == is_pid(P), whereis(?server)),
		?_assertMatch(P when true == is_pid(P), whereis(?sm)),
		?_assertMatch(P when true == is_pid(P), whereis(?ssp)),
		?_assertMatch(P when true == is_pid(P), whereis(?rh))
	       ]
       end
      }
    }.

start() ->
    application:load(genrs),
    application:start(genrs),
    ?assertMatch(P when true == is_pid(P), whereis(?sm)).

stop() ->
    application:stop(genrs),
    application:unload(genrs).


after_each(_) ->
    ?MODULE:stop().
