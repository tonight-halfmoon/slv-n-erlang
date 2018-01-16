-module(ait).

-export([run_suite/0, start/0, stop/0]).

-export([after_each/1]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("genrs/include/genrs_internal.hrl").
-include_lib("genrs/include/config.hrl").
-include_lib("genrs/include/amqp_connect.hrl").

-define(rs, 'rs.gsrp.999').

run_suite() ->
    eunit:test([?MODULE], [verbose, {report, {eunit_surefire, [{dir, "."}]}}]).

start_test_() ->
    {
      "When GenRS is loaded and started, then the Supervision Tree must be built, and the following process must have been started: `genrs_server`, `service_manager`, `service_stats_provider` and `rh`",
      {
	setup, 
	fun() -> start() end,
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
      "When a client asks to free up a resource which is allocated, then GenRS must free up the resource",
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
      "When a client asks to alloc a resource and there is no free resources, then Server must reply with a message",
      {
	setup,
	fun() -> start(),
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

dstats_test_() ->
    {
      "When a client asks for statistics on the current state of the resources data, then Server must come up with statistics results: Server must send the result to an exchange on RabbitMQ broker queue. The client who has subscribed to the same queue will be able to receive the message in an asynchronous fashion.",
      setup,
      fun() ->
	      application:ensure_all_started(amqp_service_provider),
	      application:ensure_all_started(genrs_client),
	      receive after 500 -> ok end,
	      start(),
	      genrs_amqp_consumer:request_genrs_data_statistics(),
	      receive after 500 -> ok end,
	      genrs_amqp_consumer:cask_last_amqp_msg()
      end,
      fun ?MODULE:after_each/1,
      fun(Actual) ->
	      ?_assertMatch({dstats, {bse, free, Fstats}, {bse, allocated, Astats}} when {Fstats, Astats} =:= {1,0}, Actual) end
    }.
