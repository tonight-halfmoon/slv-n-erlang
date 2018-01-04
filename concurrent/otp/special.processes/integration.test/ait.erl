-module(ait).
-export([start/0, stop/0, run_suite/0, after_each/1]).
-include("../include/config.hrl").
-include("../include/telecommunication.hrl").
-include("../src/resa_server.hrl").
-include("../src/sm.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(res, 'ap.rapp.109').

start_test_() ->
    {
      "When Application has just been loaded and started, then the following processes must be running: Resa Server, Resource Handler, Service Manager, and Stats Provider",
      {
	setup,
	fun() -> start() end,
	fun ?MODULE:after_each/1,
	fun(Actual) ->
		[
		 ?_assertEqual(ok, Actual),
		 ?_assertEqual(true, is_process_alive(whereis(?server))),
		 ?_assertEqual(true, is_process_alive(whereis(?handler))),
		 ?_assertEqual(true, is_process_alive(whereis(?sm))),
		 ?_assertEqual(true, is_process_alive(whereis(?ssp)))
		]
	end
      }
    }.

start() ->
    Ret = application:load(rssp),
    io:format("Ret: ~p~n", [Ret]),
    SRet = application:start(rssp),
    io:format("SRet: ~p~n", [SRet]).

stop_test_() ->
    {
      "When Application stops and got unloaded, then the following processes must have been stopped: Resa Server, Resource Handler, Service Manager and Stats Provider",
      {
	setup,
	fun() -> start(), stop() end,
	fun(Actual) ->
		[
		 ?_assertEqual(true, Actual),
		 ?_assertEqual(undefined, whereis(?server)),
		 ?_assertEqual(undefined, whereis(?handler)),
		 ?_assertEqual(undefined, whereis(?sm)),
		 ?_assertEqual(undefined, whereis(?ssp))
		]
	end
      }
    }.

stop() ->
    Ret = application:stop(rssp),
    io:format("Ret: ~p~n", [Ret]),
    SRet = application:unload(rssp),
    io:format("SRet: ~p~n", [SRet]),
    true.

run_suite() ->
    eunit:test([?MODULE], [verbose, {report, {eunit_surefire, [{dir, "."}]}}]).

after_each(_) ->
    ?MODULE:stop().

freeup_not_allocated_test_() ->
    { 
      "When Server receives protocol 'freeup' and a resource name and the resource has not been allocated, then Server must reply with an error message",
      {
	setup,
	fun() -> start(),
		 ?server ! #cask2free{client_pid=self(),
				      resource=term_to_binary('aitabit.19')},
		 receive
		     Msg ->
			 Msg
		 end
	end,
	fun ?MODULE:after_each/1,
	fun(Actual) ->
		?_assertEqual({server_reply, {error, not_allocated}}, Actual) end
      }
    }.

freeup_test_() ->
    {
      "When Server receives protocol 'freeup' and a resource name and the resource is allocated, then Server must free up the allocated resource",
      {
	setup,
	fun() -> start(),
		 ?server ! #cask2alloc{client_pid=self()},
		 receive
		     _ ->
			 true
		 end,
		 ?server ! #cask2free{client_pid=self(), resource=term_to_binary(?res)},
		 receive
		     Msg ->
			 Msg
		 end
	end,
	fun ?MODULE:after_each/1,
	fun(Actual) ->
		?_assertEqual({server_reply, {freed, ?res}}, Actual) end
      }
    }.

alloc_no_free_test_() ->
    {
      "When protocol 'alloc' is sent to Server and there is no free resources, then Server must reply with an error message",
      {
	setup,
	fun() -> start(),
		 ?server ! #cask2alloc{client_pid=self()},
		 receive
		     _ ->
			 true
		 end,
		 ?server ! #cask2alloc{client_pid=self()},
		 receive
		     Msg ->
			 Msg
		 end
	end,
	fun ?MODULE:after_each/1,
	fun(Actual) ->
		?_assertEqual({server_reply, no}, Actual) end
      }
    }.

alloc_test_() ->
    {
      "When protocol 'alloc' is received by Server and there is a free resource, then Server must allocate the free resource",
      {
	setup,
	fun() -> start(),
		 ?server ! #cask2alloc{client_pid=self()},
		 receive
		     Msg ->
			 Msg
		 end
	end,
	fun ?MODULE:after_each/1,
	fun(Actual) ->
		?_assertEqual({server_reply, {allocated, 'ap.rapp.109'}}, Actual) end
      }
    }.

stats_test_() ->
    {
      "When protocol 'stats' is received by Server, then Server must reply with the stats of current resources state",
      {
	setup,
	fun() -> start(),
		 ?server ! #cask4stats{client_pid=self()},
		 receive
		     Msg ->
			 Msg
		 end
	end,
	fun ?MODULE:after_each/1,
	fun(Actual) ->
		?_assertEqual({stats_reply,{stats,free,1},{stats,allocated,0}}, Actual) end
      }
    }.
