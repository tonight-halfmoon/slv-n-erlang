-module(minimal_supervisor_tests).
-include_lib("eunit/include/eunit.hrl").
-include("server.hrl").
-include("minimal_supervisor.hrl").

-export([setup_transient_children/1, after_each/1]).

restart_transient_children_have_terminated_normally_test_() ->
    {"Restart Children: When child is transient and it terminates normally, then it is not restarted",
     {
       setup,
       fun () ->
	       setup_transient_children(1),
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

restart_transient_children_have_terminated_abnormally_test_() ->
    {"Restart Children: When child is transient and it terminates abnormally, then it is restarted",
     {
       setup,
       fun () ->
	       setup_transient_children(1),
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
    {"Start Children: When Supervisor tries to start a child whose module is not available, then Supervisor restarts the child a maximum of 5 times per minute",
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

setup_transient_children(_HowMany) ->
    ChildSpecList = [{transient, {server, start_link, []}}],
    minimal_supervisor:start_link(ChildSpecList),
    receive after 1 -> ok end.

after_each(_Args) ->
    minimal_supervisor:stop().
