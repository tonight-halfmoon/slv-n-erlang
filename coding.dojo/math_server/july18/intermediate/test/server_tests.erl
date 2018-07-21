-module(server_tests).
-include_lib("eunit/include/eunit.hrl").
-import(server, [start/0, stop/1]).

start_test() ->
    {ok, Pid} = start(),

    ?assert(is_process_alive(Pid)),

    {ok, noreply} = stop(Pid).

stop_test() ->
    {ok, Pid} = start(),
    ?assert(is_process_alive(Pid)),

    {ok, noreply} = stop(Pid),
    receive after 1 -> ok end,

    ?assertNot(is_process_alive(Pid)).

