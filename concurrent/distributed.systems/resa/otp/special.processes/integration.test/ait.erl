-module(ait).
-export([start/0, stop/0, run_suite/0, after_each/1]).
-include("../include/config.hrl").
-include("../include/telecommunication.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(res, 'ap.rapp.109').

start() ->
    Ret = application:load(rssp),
    io:format("Ret: ~p~n", [Ret]),
    SRet = application:start(rssp),
    io:format("SRet: ~p~n", [SRet]).

stop() ->
    Ret = application:stop(rssp),
    io:format("Ret: ~p~n", [Ret]),
    SRet = application:unload(rssp),
    io:format("SRet: ~p~n", [SRet]).

run_suite() ->
    eunit:test([?MODULE], [verbose, {surefire, "dir", "."}]).

after_each(_) ->
    ?MODULE:stop().

freeup_not_allocated_test_() ->
    { "When Server receives protocol 'freeup' and a resource name and the resource has not been allocated, then Server must reply with an error message",
      {setup,
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
      }}.

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
