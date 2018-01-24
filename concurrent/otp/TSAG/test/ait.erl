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
    {ok, Pid} = genrs_riakc:start_link(),
    Pid.

after_each(_) ->
  ?MODULE:stop().

stop() ->
    true.

wro_geocheckin_test_() ->
    {
      "When .. then",
      {
	setup,
	fun() ->
		start(),
		genrs_riakc:wro_geocheckin(),
		receive after 500 -> true end
	end,
	fun ?MODULE:after_each/1,
	fun(Actual) ->
		?_assertEqual(true, Actual) end
      }
    }.
