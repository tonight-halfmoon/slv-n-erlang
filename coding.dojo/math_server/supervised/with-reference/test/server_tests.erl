-module(server_tests).

-include_lib("eunit/include/eunit.hrl").

-include("server.hrl").
-include("minimal_supervisor.hrl").

when_client_connected_then_resources_got_allocated_test() ->
    AvailableResources = [{R1 = '0x5a3_unit_test', undefined}, {R2 = '0x13f_unit_test', undefined}],
    ChildSpecList = [{transient, {server, start_link, AvailableResources}}],
    {ok, _Pid} = minimal_supervisor:start_link(ChildSpecList),
    receive after 3 -> ok end,
    Client1 = cp1,
    {ok, Client1Pid} = client:start(Client1),

    client:connect(Client1),

    ?assertEqual({reply, ?Server,[{R1, Client1Pid}, {R2, undefined}]}, server:check_resources()),

    _DisconnReply = client:disconnect(Client1),
    _StopReplyClient1 = client:stop(Client1),
    minimal_supervisor:stop(),
    receive after 3 -> ok end.

when_client_disconnected_then_resources_got_deallocated_test() ->
    AvailableResources = [{R1 = '0x5a3_unit_test', undefined}, {R2 = '0x13f_unit_test', undefined}],
    ChildSpecList = [{transient, {server, start_link, AvailableResources}}],
    minimal_supervisor:start_link(ChildSpecList),
    receive after 3 -> ok end,
    Client1 = cp1,
    {ok, Client1Pid} = client:start(Client1),
    client:connect(Client1),
    Client2 = cp2,
    {ok, Client2Pid} = client:start(Client2),
    client:connect(Client2),
    ?assertEqual({reply, ?Server,[{R1, Client1Pid}, {R2, Client2Pid}]}, server:check_resources()),

    _DiscReplyClient2 = client:disconnect(Client2),

    ?assertEqual({reply, ?Server,[{R1, Client1Pid}, {R2, undefined}]}, server:check_resources()),

    _DiscReplyClient1 = client:disconnect(Client1),
    _StopReplyClient1 = client:stop(Client1),
    _StopReplyClient2 = client:stop(Client2),
    minimal_supervisor:stop(),
    receive after 3 -> ok end.

when_resources_exhausted_then_client_is_not_connected_test() ->
    AvailableResources = [{R1 = '0x5a3_unit_test', undefined}, {R2 = '0x13f_unit_test', undefined}],
    ChildSpecList = [{trasient, {server, start_link, AvailableResources}}],
    minimal_supervisor:start_link(ChildSpecList),
    receive after 3 -> ok end,
    Client1 = cp1,
    {ok, Client1Pid} = client:start(Client1),
    client:connect(Client1),
    Client2 = cp2,
    {ok, Client2Pid} = client:start(Client2),
    client:connect(Client2),
    ?assertEqual({reply, ?Server,[{R1, Client1Pid}, {R2, Client2Pid}]}, server:check_resources()),
    client:disconnect(Client2),
    ?assertEqual({reply, ?Server,[{R1, Client1Pid}, {R2, undefined}]}, server:check_resources()),
    client:connect(Client2),
    Client3 = cp3,
    {ok, Client3Pid} = client:start(Client3),

    client:connect(Client3),

    ?assertEqual({reply, ?Server, [{R1, Client1Pid}, {R2, Client2Pid}]}, server:check_resources()),
    ?assertEqual({reply, Client3Pid, not_yet}, client:is_connected(Client3)),

    client:disconnect(Client1),
    client:disconnect(Client2),
    client:disconnect(Client3),
    client:stop(Client1),
    client:stop(Client2),
    client:stop(Client3),
    minimal_supervisor:stop(),
    receive after 3 -> ok end.
