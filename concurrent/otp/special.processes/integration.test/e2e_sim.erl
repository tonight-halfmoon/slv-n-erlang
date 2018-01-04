-module(e2e_sim).
-export([start/0, start_resa/0, stop/0, stop_resa/1, stop_sm/1, run_suite/0, start_sm/0, freeup/0, alloc/0, stats/0, after_each/1, before_each/0]).
-include("../include/config.hrl").
-include("../src/resa_server.hrl").
-include("../include/telecommunication.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(sr, 'ab.12').

start() ->
    sm:start_link(),
    resa_server:start_link([?sr]).

start_resa() ->
    [resa_server:start_link([?sr])].

start_sm() ->
    [sm:start_link()].

stop() ->
    resa_server:stop(),
    sm:stop().

stop_resa(_) ->
    resa_server:stop().

stop_sm(_) ->
    sm:stop(),
    ok.

run_suite() ->
    eunit:test([?MODULE], [verbose, {report,{eunit_surefire, [{dir, "."}]} }]).

before_each() ->
    [start()].

after_each(_) ->
    stop().

alloc() ->
    ?server ! #cask2alloc{client_pid=self()},
    receive
	Msg ->
	    io:format("Received: ~p~n", [Msg])
    end.

freeup() ->
    ?server ! #cask2free{client_pid=self(), resource=term_to_binary('ab.12')},
    receive
	Msg ->
	    io:format("Received: ~p~n", [Msg])
    end.

stats() ->
    ?server ! #cask4stats{client_pid=self()},
    receive
	Msg ->
	    io:format("Received: ~p~n", [Msg])
    end.

%%% TODO:
%%%  1 - clean up has been invoked!

stop_handler_test_()->
    {
      "When Handler stops, then RESA Server must stop, too",
      {setup,
       fun ?MODULE:start_resa/0, 
       %fun ?MODULE:stop_sm/1,
       fun([{ok, Resa_pid}]) ->
	       ?handler ! {stop, protocol_tests, self()},
	       receive _ -> true after 100 -> true end,
	       [?_assertEqual(undefined, whereis(?handler)),
		?_assertEqual(false, is_process_alive(Resa_pid))]
       end
      }
    }.

stop_stats_test_() ->
    {
      "When protocol 'stop' has been received by 'service_stats_provider', then it must stop and Service Manager must stop, too",
      {setup,
       fun ?MODULE:start_sm/0,
       %% [{sm_running, Sm_pid}]
       fun([{ok, Sm_pid}]) ->
	       ?ssp ! {stop, protocol_tests, self()},
	       receive
		   _-> true after 100 -> true end,
	       [?_assertEqual(undefined, whereis(?ssp)),
		?_assertEqual(false, is_process_alive(Sm_pid))]
       end
      }
    }.
