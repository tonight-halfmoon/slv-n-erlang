-module(server_tests).
-include_lib("eunit/include/eunit.hrl").
-import(server, [start/0, stop/0]).
-include("server.hrl").

start_test() ->
    {ok, noreply} = start(),

    ?assertMatch(Pid when is_pid(Pid), whereis(?Server)),
    ?assert(is_process_alive(whereis(?Server))),

    {ok, noreply} = stop().

stop_test() ->
    {ok, noreply} = start(),

    {ok, noreply} = stop(),
    receive after 1 -> ok end,

    ?assertEqual(undefined, whereis(?Server)).

