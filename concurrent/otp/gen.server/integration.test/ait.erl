-module(ait).

-export([run_suite/0, start/0, stop/0]).

-export([after_each/1]).

-include_lib("eunit/include/eunit.hrl").

-include("../src/sm.hrl").
-include("../src/sp.hrl").
-include("../src/rh.hrl").
-include("../src/genrs.hrl").
-include("../include/config.hrl").

-define(rs, 'rs.gsrp.999').

run_suite() ->
    eunit:test([?MODULE], [verbose, {report, {eunit_surefire, [{dir, "."}]}}]).

start_test_() ->
    {
      "When GenRS is loaded and started, then the Supervision Tree must be built, and the following process must have been started: `genrs_server`, `service_manager`, `service_stats_provider` and `rh`",
      {
	setup, 
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

freeup_resource_has_not_been_allocated_test_() ->
    {
      "When Server receives protocol 'freeup' and a resource name and the resource has not been allocated, then Server must reply with an error message",
      {
	setup,
	fun() -> start(),
		 genrs:freeup(term_to_binary('res.has.not.been.allocated.8741')),
		 receive
		     Msg ->
			 Msg
		 end
	end,
	fun ?MODULE:after_each/1,
	fun(Actual) ->
		?_assertEqual({error,'resource has not been allocated'}, Actual) end
      }
    }.

%%% This Test Case also implicitly tests alloc success.
freeup_test_() ->
    {
      "When client asks to free up a resource which is allocated, then GenRS must free up the resource",
      {
	setup,
	fun() -> start(),
		 genrs:alloc(),
		 receive
		     _ ->
			 pass
		 end,
		 genrs:freeup(term_to_binary(?rs)),
		 receive
		     Msg ->
			 Msg
		     end
	end,
	fun ?MODULE:after_each/1,
	fun(Actual) ->
		?_assertEqual(#ok{more={'free up success', term_to_binary(?rs)}}, Actual) end
      }
    }.

alloc_no_free_resource_test_() ->
    {
      "When client asks to alloc a resource and there is no free resources, then Server must reply with a message",
      {
	setup,
	fun() -> ?MODULE:start(),
		 genrs:alloc(),
		 receive 
		     _ ->
			 pass
		 end,
		 genrs:alloc(),
		 receive
		     Msg ->
			 Msg
		 end
	end,
	fun ?MODULE:after_each/1,
	fun(Actual) ->
		?_assertEqual({error, 'no free resource anymore'}, Actual) end
	
      }
    }.

stats_test_() ->
    {
      "When client asks for statistics on the current state of the resources, then Server must reply with statistics results",
      setup,
      fun() -> ?MODULE:start(),
	       genrs:cask_stats(),
	       receive
		   Msg ->
		       Msg
		   end
      end,
      fun ?MODULE:after_each/1,
      fun(Actual) ->
	      ?_assertMatch(#ok{more={stats, {free, Fstats}, {allocated, Astats}}} when {Fstats,Astats} =:= {1,0}, Actual) end
    }.
