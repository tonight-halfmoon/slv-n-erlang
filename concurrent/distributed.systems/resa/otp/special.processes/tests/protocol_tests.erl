-module(protocol_tests).
-export([before_each/0, after_each/1]).
-include("../config/config.hrl").
-include("../server/config_internal.hrl").
-include("../config/telecommunication.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(sr, 'ab.12').

before_each() ->
    resa_server:start_link([?sr]).

after_each(_) ->
    resa_server:stop().

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
      "When protocol 'freeup' is sent to Server with a resource name provided and the resource is allocated, then the resource must be freedup",
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
    {"When protocol 'cask4stats' is sent to Server, then stats about the free and allocated resources must be returned",
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
    {"When Handler stops, then RESA Server must stop, too",
     {setup,
      fun ?MODULE:before_each/0,
      fun({ok, RESA_server_pid}) ->
	      ?handler ! {stop, protocol_tests, self()},
	      receive _ -> true after 100 -> true end,
	      [?_assertEqual(undefined, whereis(?handler)),
	       ?_assertEqual(false, is_process_alive(RESA_server_pid))]
      end
     }
    }.

stop_stats_test_() ->
    {"When protocol 'stop' has been received by 'stats', thehn RESA server must stop",
     {setup,
      fun ?MODULE:before_each/0,
      fun({ok, RESA_Server_pid}) ->
	      ?ssp ! {stop, protocol_tests, self()},
	      receive
		  _-> true after 100 -> true end,
	      [?_assertEqual(undefined, whereis(?ssp)),
	       ?_assertEqual(false, is_process_alive(RESA_Server_pid))]
      end
     }
    }.
