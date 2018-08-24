-module(minimal_supervisor_tests).
-include_lib("eunit/include/eunit.hrl").
-include("server.hrl").
-include("minimal_supervisor.hrl").

-export([after_each/1]).

-define(ServerChildTransientSpec, {transient, {server, start_link, []}}).
-define(ServerChildPermanentSpec, {permanent, {server, start_link, []}}).

start_child_given_child_spec_when_supervisor_tries_to_start_child_then_child_started_test_() ->
    {
     "Start Child; Given a child spec, when the supervisor tries to start a new child process, then the child is started",
     {
      setup,
      fun() ->
	      minimal_supervisor:start_link([]),
	      receive after 1 -> ok end,
	      {reply, ?Supervisor, {ok, child_started, {child_state, ChildPid, ChildId, ?ServerChildTransientSpec}}} = minimal_supervisor:start_child(?ServerChildTransientSpec),
	      {ChildId, ChildPid}
      end,
      fun ?MODULE:after_each/1,
      fun({_ChildId, ChildPid}) ->
	      [
	       ?_assert(is_process_alive(ChildPid))
	      ]
      end
     }
    }.

start_child_the_supervisor_is_able_to_start_children_even_once_the_supervisor_has_started_test_() ->
    {
      "Start Child; Given child spec, when the supervisor has started, then the supervisor starts the child and returns a unique id",
      {
	setup,
	fun() ->
		minimal_supervisor:start_link([]),
		receive after 1 -> ok end
	end,
	fun ?MODULE:after_each/1,
	fun(_) ->
		Result = minimal_supervisor:start_child(?ServerChildTransientSpec),

		[
		 ?_assertMatch({reply, ?Supervisor, {ok, child_started, {child_state, ChildPid, _ChildId, ?ServerChildTransientSpec}}} when is_pid(ChildPid), Result)
		]
	end
      }
    }.

stop_child_given_child_id_when_supervisor_tries_to_stop_child_then_child_stopped_test_() ->
    {
     "Stop Child; Given a child Id, when the supervisor tries to stop the child, then the child is stopped",
     {
      setup,
      fun() ->
	      minimal_supervisor:start_link([]),
	      receive after 1 -> ok end,
	      {reply, ?Supervisor, {ok, child_started, {child_state, ChildPid, ChildId, ?ServerChildPermanentSpec}}} = minimal_supervisor:start_child(?ServerChildPermanentSpec),
	      ?assert(is_process_alive(ChildPid)),
	      {ChildId, ChildPid}
      end,
      fun ?MODULE:after_each/1,
      fun({ChildId, ChildPid}) ->
	      minimal_supervisor:stop_child(ChildId),
	      receive after 3 -> ok end,

	      [?_assertNot(is_process_alive(ChildPid))]
      end
     }
    }.

stop_child_given_child_started_during_supervisor_startup_and_the_child_restarted_when_the_supervisor_tries_to_stop_the_child_then_the_child_stopped_test_() ->
    {
     "Start Child: Given a child started during supervisor startup, and the child terminated and restarted, when the supervisor tries to stop the child then the child is stopped",
     {
      setup,
      fun() ->
	      minimal_supervisor:start_link([?ServerChildTransientSpec]),
	      receive after 3 -> ok end,
	      ChildPid = whereis(?Server),
	      ?assert(is_process_alive(ChildPid)),
	      {ok, child_found, ChildId, ChildPid} = minimal_supervisor:keyfind(id, ChildPid),
	      exit(ChildPid, kill),
	      receive after 3 -> ok end,
	      ChildPidAfterRestart = whereis(?Server),
	      ?assert(is_process_alive(ChildPidAfterRestart)),
	      {ok, child_found, ChildId, ChildPidAfterRestart} = minimal_supervisor:keyfind(id, ChildPidAfterRestart),
	      {ChildId, ChildPidAfterRestart}
      end,
      fun ?MODULE:after_each/1,
      fun({ChildId, ChildPid}) ->
	      minimal_supervisor:stop_child(ChildId),

	      [?_assertNot(is_process_alive(ChildPid))]
      end
     }
    }.

stop_child_given_child_id_when_supervisor_stops_the_child_then_the_child_is_deleted_from_the_state_of_the_supervisor_test_() ->
    {
     "Stop Child; Given a child Id, when the supervisor stops the child, then child is stopped and deleted from the state of the supervisor",
     {
      setup,
      fun() ->
	      minimal_supervisor:start_link([]),
	      {reply, ?Supervisor, {ok, child_started, {child_state, ChildPid, ChildId, ?ServerChildPermanentSpec}}} = minimal_supervisor:start_child(?ServerChildPermanentSpec),
	      ?assert(is_process_alive(ChildPid)),
	      {ChildId, ChildPid}
      end,
      fun ?MODULE:after_each/1,
      fun({ChildId, ChildPid}) ->
	      minimal_supervisor:stop_child(ChildId),
	      FindChildResult = minimal_supervisor:keyfind(spec, ChildId),

	      [
	       ?_assertNot(is_process_alive(ChildPid)),
	       ?_assertMatch({error, child_not_found, ChildId}, FindChildResult)
	      ]
      end
     }
    }.

exit_child_with_reason_killed_given_child_id_when_a_permanent_child_exits_as_killed_and_restarted_then_the_child_remain_in_the_state_of_the_supervisor_having_the_same_id_test_() ->
    {
     "Stop Child; Given a child Id, when a ´permanent´ child has exited with reason ´killed´ and the supervisor restarted the child, then the child remains in the state of the supervisor having the same child Id",
     {
      setup,
      fun () ->
	      minimal_supervisor:start_link([]),
	      {reply, ?Supervisor, {ok, child_started, {child_state, ChildPid, ChildId, ?ServerChildPermanentSpec}}} = minimal_supervisor:start_child(?ServerChildPermanentSpec),
	      ?assert(is_process_alive(ChildPid)),
	      {ok, child_found, ChildId, ChildPid} = minimal_supervisor:keyfind(pid, ChildId),
	      {ChildId, ChildPid}
      end,
      fun ?MODULE:after_each/1,
      fun({ChildId, ChildPid}) ->
	      %{error, child_not_stopped, ChildId, ChildPid} = minimal_supervisor:stop_child(ChildId),
	      exit(ChildPid, kill),
	      receive after 3 -> ok end,
	      {ok, child_found, ChildId, ChildPidAfterRestart} = minimal_supervisor:keyfind(pid, ChildId),

	      [
	       ?_assertNotEqual(ChildPid, ChildPidAfterRestart)
	      ]
      end
     }
    }.

exit_child_with_reason_killed_given_child_id_when_a_transient_child_exits_as_killed_and_restarted_then_the_child_remain_in_the_state_of_the_supervisor_having_the_same_id_test_() ->
    {
     "Stop Child; Given a child Id, when a ´transient´ child has exited with reason ´killed´ and the supervisor restarted the child, then the child remains in the state of the supervisor having the same child Id",
     {
      setup,
      fun () ->
	      minimal_supervisor:start_link([]),
	      {reply, ?Supervisor, {ok, child_started, {child_state, ChildPid, ChildId, ?ServerChildTransientSpec}}} = minimal_supervisor:start_child(?ServerChildTransientSpec),
	      ?assert(is_process_alive(ChildPid)),
	      {ok, child_found, ChildId, ChildPid} = minimal_supervisor:keyfind(pid, ChildId),
	      {ChildId, ChildPid}
      end,
      fun ?MODULE:after_each/1,
      fun({ChildId, ChildPid}) ->
	      exit(ChildPid, kill),
	      receive after 3 -> ok end,
	      {ok, child_found, ChildId, ChildPidAfterRestart} = minimal_supervisor:keyfind(pid, ChildId),

	      [
	       ?_assertNotEqual(ChildPid, ChildPidAfterRestart)
	      ]
      end
     }
    }.

%% restart_children_when_transient_child_terminated_normally_then_child_is_not_restarted_test_() ->
%%     {"Restart Children; Given a transient child, when the child terminates normally, then it is not restarted",
%%      {
%%        setup,
%%        fun() ->
%% 	       ChildSpecList = [{transient, {server, start_link, []}}],
%% 	       minimal_supervisor:start_link(ChildSpecList),
%% 	       receive after 3 -> ok end,
%% 	       ServerPid1 = whereis(?Server),
%% 	       exit(ServerPid1, normal),
%% 	       receive after 1 -> ok end,
%% 	       ServerPid2 = whereis(?Server),
%% 	       {ServerPid1, ServerPid2}
%%        end,
%%        fun ?MODULE:after_each/1,
%%        fun({ServerPid1, undefined}) ->
%% 	       [
%% 		?_assertNot(is_process_alive(ServerPid1))
%% 	       ]
%%        end
%%      }
%%     }.

%%restart_children_when_transient_child_terminated_abnormally_then_child_is_restarted_test_() ->
    %% {"Restart Children; Given a transient child, when the child terminates abnormally, then it is restarted",
    %%  {
    %%    setup,
    %%    fun () ->
    %% 	       ChildSpecList = [{transient, {server, start_link, []}}],
    %% 	       {ok, supervisor_started} = minimal_supervisor:start_link(ChildSpecList),
    %% 	       receive after 3 -> ok end,
    %% 	       ServerPid1 = whereis(?Server),
    %% 	       exit(ServerPid1, kill),
    %% 	       ServerPid2 = whereis(?Server),
    %% 	       {ServerPid1, ServerPid2}
    %%    end,
    %%    fun ?MODULE:after_each/1,
    %%    fun({ServerPid1, ServerPid2}) ->
    %% 	       [
    %% 		?_assertNotEqual(ServerPid1, ServerPid2),
    %% 		?_assert(is_process_alive(ServerPid2))
    %% 	       ]
    %%    end
    %%  }
    %% }.

start_children_given_unavailable_child_spec_module_when_supervisor_tries_to_start_the_child_then_the_supervisor_tries_5_times_per_minute_test_() ->
    {"Start Children; Given unavailable module of child spec, When the Supervisor tries to start then child, then the Supervisor restarts the child a maximum of 5 times per minute",
     {
       setup,
       fun() ->
	       minimal_supervisor:start_link([{transient, {unavailable_module, f, []}}]),
	       receive after ?TimeoutOriginalValue -> ok end
       end,
       fun ?MODULE:after_each/1,
       fun(_Actual) ->
	       []
       end
     }
    }.

stop_child_given_child_id_when_supervisor_tries_to_stop_the_child_then_the_child_is_stopped_test_() ->
    {
      "Stop Child; Given a child Id, when the supervisor tries to stop the child, then the the child is stopped and removed from the child list of the supervisor's state",
      {
	setup,
	fun() ->
		minimal_supervisor:start_link([]),
		{reply, ?Supervisor, {ok, child_started, {child_state, ChildPid, ChildId, _ChildSpec}}} = minimal_supervisor:start_child({transient, {server, start_link, []}}),
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

stop_when_supervisor_stop_then_transient_children_stop_test() ->
    ChildSpecList = [?ServerChildTransientSpec],
    minimal_supervisor:start_link(ChildSpecList),
    receive after 3 -> ok end,
    ?assert(is_process_alive(whereis(?Server))),

    minimal_supervisor:stop(),
    receive after 3 -> ok end,

    ?assertEqual(undefined, whereis(?Server)).

permanent_children_restarted_upon_abnormal_termination_test() ->
    ChildSpecList = [?ServerChildPermanentSpec],
    minimal_supervisor:start_link(ChildSpecList),
    receive after 3 -> ok end,
    ServerPid = whereis(?Server),
    ?assert(is_process_alive(ServerPid)),

    exit(ServerPid, kill),
    receive after 1 -> ok end,
    ServerPid2 = whereis(?Server),

    ?assertNotEqual(ServerPid, ServerPid2),
    ?assert(is_process_alive(ServerPid2)),

    minimal_supervisor:stop().

permanent_children_restarted_upon_normal_termination_test() ->
    ChildSpecList = [?ServerChildPermanentSpec],
    minimal_supervisor:start_link(ChildSpecList),
    receive after 3 -> ok end,
    ServerPid = whereis(?Server),
    ?assert(is_process_alive(ServerPid)),

    exit(ServerPid, normal),
    receive after 3 -> ok end,
    ServerPid2 = whereis(?Server),

    ?assertNotEqual(ServerPid, ServerPid2),
    ?assert(is_process_alive(ServerPid2)),

    minimal_supervisor:stop().

permanent_children_stop_when_supervisor_stop_test() ->
    ChildSpecList = [?ServerChildPermanentSpec],
    minimal_supervisor:start_link(ChildSpecList),
    receive after 3 -> ok end,
    ?assert(is_process_alive(whereis(?Server))),

    minimal_supervisor:stop(),
    receive after 3 -> ok end,

    ?assertEqual(undefined, whereis(?Server)).

handle_unavailable_child_module_test() ->
    ChildSpecList = [ {transient, {module_unavailable, funct, []}}],
    minimal_supervisor:start_link(ChildSpecList),

    minimal_supervisor:stop(),
    receive after 2 -> ok end,

    ?assertEqual(undefined, whereis(?Server)).
