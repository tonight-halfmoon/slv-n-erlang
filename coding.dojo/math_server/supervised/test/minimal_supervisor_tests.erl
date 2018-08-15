-module(minimal_supervisor_tests).
-include_lib("eunit/include/eunit.hrl").
-include("server.hrl").
-include("minimal_supervisor.hrl").

start_children_test() ->
    ChildSpecList = [{server, start_link, []}, {asd,d,[]}],
    {ok, _Pid} = dj_supervisor:start_link(ChildSpecList),
    receive after 1 -> ok end,
    ServerPid = whereis(?Server),

    ?assert(is_process_alive(ServerPid)),

    {reply, _Pid, {ok, children_terminated}} = dj_supervisor:stop().

stop_children_test() ->
    ChildSpecList = [{server, start_link, []}, {asd, asd, []}],
    dj_supervisor:start_link(ChildSpecList),
    receive after 1 -> ok end,
    ?assert(is_process_alive(whereis(?Server))),

    minimal_supervisor:stop(),
    receive after 1 -> ok end,

    ?assertEqual(undefined, whereis(?Server)).

restart_children_test() ->
    ChildSpecList = [{server, start_link, []}, {sdf, sd, []}],
    minimal_supervisor:start_link(ChildSpecList),
    receive after 1 -> ok end,
    ServerPid = whereis(?Server),
    ?assert(is_process_alive(ServerPid)),

    exit(whereis(?Server), kill),
    receive after 1 -> ok end,
    ServerPid2 = whereis(?Server),

    ?assertNotEqual(ServerPid, ServerPid2),
    ?assert(is_process_alive(ServerPid2)),

    minimal_supervisor:stop().
