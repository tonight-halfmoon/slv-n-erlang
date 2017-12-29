-module(e2e_sim).
-export([start/0, stop/0, freeup/0, alloc/0, stats/0, after_each/1, before_each/0]).
-include("../include/config.hrl").
-include("../src/resa_server.hrl").
-include("../include/telecommunication.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(sr, 'ab.12').

start() ->
    sm:spawn_link(),
    resa_server:start_link([?sr]).

stop() ->
    resa_server:stop(),
    sm:stop().

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

stop_handler_test_()->
    {
      "When Handler stops, then RESA Server must stop, too",
      {setup,
       fun ?MODULE:before_each/0,
       fun([_, {ok, Resa_pid}]) ->
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
      fun ?MODULE:before_each/0,
       fun([Sm_pid, _]) ->
	       ?ssp ! {stop, protocol_tests, self()},
	       receive
		   _-> true after 100 -> true end,
	       [?_assertEqual(undefined, whereis(?ssp)),
		?_assertEqual(false, is_process_alive(Sm_pid))]
       end
      }
    }.
