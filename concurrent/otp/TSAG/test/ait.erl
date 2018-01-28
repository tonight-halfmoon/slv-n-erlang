%%%-------------------------------------------------------------------
%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% On the 24th of January 2018
%%%-------------------------------------------------------------------
-module(ait).

-export([run_suite/0, stop/0, after_each/1]).

-include_lib("eunit/include/eunit.hrl").

run_suite() ->
    eunit:test([?MODULE], [verbose, {report, {eunit_surefire, [{dir, "."}]}}]).

start() ->
    %% TODO: Handle case when Riak is not reachable at start up
    {ok, Pid} = genrs_riakc:start_link(),
    Pid.

after_each(_) ->
  ?MODULE:stop().

stop() ->
    {}.

wro_geocheckin_test_() ->
    { % Pre-Condition: Riak has been started
      % TODO: automate start-up for a riak docker with table 'GeoCheckin'
      "When function `wro_geocheckin` of module `genrs_raikc` is called, then a new row at RIAK TS in table `GeoCheckin` is created",
      {
	setup,
	fun() ->
		start(),
		genrs_riakc:wro_geocheckin(),
		receive after 500 -> true end,
	        genrs_riakc:r_geocheckin()
	end,
	fun ?MODULE:after_each/1,
	fun(Actual) ->
		?_assertEqual({ok, {[<<"weather">>,<<"temperature">>],
				    [{<<"extremely hot">>, 33.4}]}}, Actual) end
      }
    }.

%% TODO: Test Case for when {error,{1019,<<"GeoCheckin is not an act"...>>}}
%% TODO: Test Case when Riak has not been started
