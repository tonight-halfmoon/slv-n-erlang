-module(minimal_supervisor_tests).
-include_lib("eunit/include/eunit.hrl").
-include("server.hrl").
-include("minimal_supervisor.hrl").

-export([after_each/1]).

restart_children_when_transient_child_has_terminated_normally_test_() ->
    {"Restart; When a child is transient and it terminates normally, then it is not restarted",
     {
       setup,
       fun () ->
	       ChildSpecList = [{transient, {server, start_link, []}}],
	       minimal_supervisor:start_link(ChildSpecList),
	       receive after 3 -> ok end,
	       ServerPid1 = whereis(?Server),
	       exit(ServerPid1, normal),
	       receive after 3 -> ok end,
	       ServerPid2 = whereis(?Server),
	       {ServerPid1, ServerPid2}
       end,
       fun ?MODULE:after_each/1,
       fun({ServerPid1, undefined}) ->
	       [
		?_assertNot(is_process_alive(ServerPid1))
	       ]
       end
     }
    }.

restart_children_when_transient_child_has_terminated_abnormally_test_() ->
    {"Restart; When a child is transient and it terminates abnormally, then it is restarted",
     {
       setup,
       fun () ->
	       ChildSpecList = [{transient, {server, start_link, []}}],
	       minimal_supervisor:start_link(ChildSpecList),
	       receive after 3 -> ok end,
	       ServerPid1 = whereis(?Server),
	       exit(ServerPid1, kill),
	       receive after 3 -> ok end,
	       ServerPid2 = whereis(?Server),
	       {ServerPid1, ServerPid2}
       end,
       fun ?MODULE:after_each/1,
       fun({ServerPid1, ServerPid2}) ->
	       [
		?_assertNotEqual(ServerPid1, ServerPid2),
		?_assert(is_process_alive(ServerPid2))
	       ]
       end
     }
    }.

start_children_when_child_module_not_available_test_() ->
    {"Start Children; When the Supervisor tries to start a child whose module is not available, then the Supervisor restarts the child a maximum of 5 times per minute",
     {
       setup,
       fun() ->
	       minimal_supervisor:start_link([{transient, {unavailable_module, f, []}}]),
	       receive after 6003 -> ok end
       end,
       fun ?MODULE:after_each/1,
       fun(_Actual) ->
	       []
       end
     }
    }.

start_child_the_supervisor_is_able_to_start_children_even_once_the_supervisor_has_started_test_() ->
    {
      "Start Child; When Child spec is provided and the supervisor has started, then the supervisor starts the child and returns a unique id",
      {
	setup,
	fun() ->
		minimal_supervisor:start_link([])
	end,
	fun ?MODULE:after_each/1,
	fun(_) ->
		Result = minimal_supervisor:start_child({transient, {server, start_link, []}}),
		[?_assertMatch({reply, ?Supervisor, {ok, Id, _ChildPid}} when not is_pid(Id), Result)]
	end
      }
    }.

stop_child_supervisor_able_to_stop_child_given_id_test_() ->
    {
      "Stop Child; When a Child Id is provided and the supervisor has started, then he supervisor stops the child and remove it from the child list",
      {
	setup,
	fun() ->
		minimal_supervisor:start_link([]),
		{reply, ?Supervisor, {ok, child_started, {child_state, ChildPid, ChildId, _ChildSpec}}} = minimal_supervisor:start_child({transient, {server, start_link, []}}),
		receive after 3 -> ok end,
		?assert(is_process_alive(ChildPid)),
		%Result = minimal_supervisor:stop_child(ChildId),
		%{Result, ChildPid}
		{ChildId, ChildPid}
	end,
	fun ?MODULE:after_each/1,
	%fun({{reply, ?Supervisor, {ok, child_stopped, _ChildSpec}}, ChildPid}) ->
	fun({ChildId, ChildPid}) ->
		minimal_supervisor:stop_child(ChildId),
		receive after 3 -> ok end,
		?_assertNot(is_process_alive(ChildPid))
	end
      }
    }.

after_each(_Args) ->
    minimal_supervisor:stop().
