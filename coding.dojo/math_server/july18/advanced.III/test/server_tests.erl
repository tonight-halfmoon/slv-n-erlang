-module(server_tests).
-include_lib("eunit/include/eunit.hrl").
-include("server.hrl").

start_test() ->
    {ok, noreply} = server:start(),

    ?assertMatch(Pid when is_pid(Pid), whereis(?Server)),
    ?assert(is_process_alive(whereis(?Server))),

    {ok, noreply} = server:stop().

stop_test() ->
    {ok, noreply} = server:start(),
    ?assert(is_process_alive(whereis(?Server))),

    {ok, noreply} = server:stop(),
    receive after 1 -> ok end,

    ?assertEqual(undefined, whereis(?Server)).

allocate_resource_when_client_connect_test() ->
    Available = [{'0x05', undefind}, {'0xa', undefined}],
    {ok, noreply} = server:start(Available),
    {ok, Pid} = client:start(),

    {ok, noreply} = client:connect(),
    receive after 1 -> ok end,

    ?assertMatch({reply, ?Server, [_Other, {'0xa', Pid}]} when is_pid(Pid), server:allocated()),

    {ok, noreply} = server:stop().

