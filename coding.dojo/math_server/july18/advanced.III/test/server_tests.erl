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
    AvailableResources = [{'0x05', undefind}, {'0xa', undefined}],
    {ok, noreply} = server:start(AvailableResources),
    {ok, Pid} = client:start(),

    {ok, noreply} = client:connect(),
    receive after 1 -> ok end,

    ?assertMatch({reply, ?Server, [_Other, {'0xa', Pid}]} when is_pid(Pid), server:check_resources()),

    {ok, noreply} = server:stop().

deallocate_resource_when_client_disconnect_test() ->
    AvailableResources = [{'0x5a3', undefined}, {'0x13', undefined}],
    {ok, noreply} = server:start(AvailableResources),
    {ok, _Pid} = client:start(),
    {ok, noreply} = client:connect(),
    receive after 1 -> ok end,

    {ok, noreply} = client:disconnect(),
    receive after 1 -> ok end,

    ?assertEqual({reply, ?Server, AvailableResources}, server:check_resources()),

    {ok, noreply} = server:stop().

when_all_resources_exhausted_then_client_is_connected_test() ->
     AvailableResources = [{R1 = '0x5a3', undefined}, {R2 = '0x13f', undefined}],
    {ok, noreply} = server:start(AvailableResources),
    Client1 = cp1,
    {ok, Pid1} = client:start(Client1),
    {ok, noreply} = client:connect(Client1),
    receive after 1 -> ok end,
    Client2 = cp2,
    {ok, Pid2} = client:start(Client2),
    {ok, noreply} = client:connect(Client2),
    receive after 1 -> ok end,

    Client3 = cp3,
    {ok, _Pid3} = client:start(Client3),
    {ok, noreply} = client:connect(Client3),
    receive after 1 -> ok end,

    ?assertEqual({reply, ?Server, [{R1, Pid1}, {R2, Pid2}]}, server:check_resources()),

    {ok, noreply} = server:stop().
