-module(protocol_tests).
-export([run_suite/0, after_each/1, before_each/0]).
-include("../include/config.hrl").
-include("../src/resa_server.hrl").
-include("../include/telecommunication.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(sr, 'ab.12').

run_suite() ->
    eunit:test(?MODULE, [verbose]).

before_each() ->
   [
    sm:start_link(),
    resa_server:start_link([?sr])
   ].

after_each(_) ->
    resa_server:stop(),
    sm:stop().

alloc_test_() ->
    {
      "When protocol 'alloc' is sent to Server and there is at least one free resource, then the resource must be allocated",
      {setup,
       fun ?MODULE:before_each/0,
       fun ?MODULE:after_each/1,
       fun(_) ->
	       ?server ! #cask2alloc{client_pid=self()},
	       receive
		   Msg ->
                       ?_assertEqual({server_reply,{allocated,'ab.12'}}, Msg)
	       end
       end
      }
    }.

freeup_test_() ->
    {
      "When protocol 'freeup' is sent to Server with a resource name provided and the resource is allocated, then the resource must be freed up",
      {setup,
       fun() -> before_each(),
		?server ! #cask2alloc{client_pid=self()},
		receive
		    _ ->
			true
		end,
		?server ! #cask2free{client_pid=self(), resource=term_to_binary('ab.12')},
		receive
		    MsgF ->
			MsgF
		end
       end,
       fun ?MODULE:after_each/1,
       fun(Actual) -> ?_assertEqual({server_reply,{freed,'ab.12'}}, Actual) end
      }}.

stats_test_() ->
    {
      "When protocol 'cask4stats' is sent to Server, then stats about the free- and allocated- resources must be returned",
      {setup,
       fun ?MODULE:before_each/0,
       fun ?MODULE:after_each/1,
       fun(_) ->
	       ?server ! #cask4stats{client_pid=self()},
	       receive
		   Msg ->
		       ?_assertEqual({stats_reply,{stats,free,1},{stats,allocated,0}}, Msg)
	       end
       end
      }
    }.

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
       fun([{ok, Sm_pid}, _]) ->
	       ?ssp ! {stop, protocol_tests, self()},
	       receive
		   _-> true after 100 -> true end,
	       [?_assertEqual(undefined, whereis(?ssp)),
		?_assertEqual(false, is_process_alive(Sm_pid))]
       end
      }
    }.
