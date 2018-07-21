-module(server_tests).
-include_lib("eunit/include/eunit.hrl").
-import(server, [start/0, stop/0]).
-include("server.hrl").

start_test() ->
    {ok, Pid} = start(),

    ?assert(is_process_alive(Pid)),
    ?assertEqual(Pid, whereis(?Server)),

    {ok, noreply} = stop().

stop_test() ->
    {ok, Pid} = start(),
    ?assert(is_process_alive(Pid)),
    ?assertEqual(Pid, whereis(?Server)),

    {ok, noreply} = stop(),
    receive after 1 -> ok end,

    ?assertEqual(undefined, whereis(?Server)),
    ?assertNot(is_process_alive(Pid)).

