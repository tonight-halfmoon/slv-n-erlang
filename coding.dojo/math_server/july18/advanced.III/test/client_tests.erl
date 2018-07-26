-module(client_tests).
-include_lib("eunit/include/eunit.hrl").
-import(client, [sum_areas/1, async_sum_areas/1]).
-include("client.hrl").

connect_test() ->
    {ok, noreply} = server:start(),
    {ok, Pid} = client:start(),
    {ok, noreply} = client:connect(),
    receive after 1 -> ok end,

    ?assertEqual(Pid, whereis(?Client)),
    ?assert(is_process_alive(whereis(?Client))),

    {ok, noreply} = client:disconnect(),
    receive after 1 -> ok end,
    {ok, noreply} = server:stop().

when_server_resources_exhausted_client_cannot_connect_and_terminate_test() ->
    {ok, noreply} = server:start(),
    Client1 = cp1,
    {ok, Pid1} = client:start(Client1),
    {ok, noreply} = client:connect(Client1),
    receive after 1 -> ok end,
    Client2 = cp2,
    {ok, Pid2} = client:start(Client2),
    {ok, noreply} = client:connect(Client2),
    receive after 1 -> ok end,

    Client3 = cp3,
    {ok, Pid3} = client:start(Client3),
    {ok, noreply} = client:connect(Client3),
    receive after 1 -> ok end,

    ?assertNot(is_process_alive(Pid3)),
    ?assert(is_process_alive(Pid2)),
    ?assert(is_process_alive(Pid1)),

    {ok, noreply} = server:stop().

disconnect_test() ->
    {ok, noreply} = server:start(),
    {ok, Pid} = client:start(),
    {ok, noreply} = client:connect(),
    receive after 1 -> ok end,
    ?assert(is_process_alive(whereis(?Client))),

    {ok, noreply} = client:disconnect(),
    receive after 1 -> ok end,

    ?assertNot(is_process_alive(Pid)),   
    ?assertEqual(undefined, (whereis(?Client))),

    {ok, noreply} = server:stop().

sum_areas_test() ->
    {ok, noreply} = server:start(),
    {ok, _Pid} = client:start(),
    {ok, noreply} = client:connect(),
    Shapes = [{circle, 3}, {rectangle, 3, 4}],

    {reply, {ok, Sum}} = sum_areas(Shapes),

    ?assertEqual(40.27433388230814, Sum),

    {ok, noreply} = server:stop().

async_sum_areas_test() ->
    {ok, noreply} = server:start(),
    {ok, _Pid} = client:start(),
    {ok, noreply} = client:connect(),
    receive after 1 -> ok end,
    Shapes = [{circle, 3}, {rectangle, 3, 4}],

    {ok, noreply} = async_sum_areas(Shapes),

    receive
	{reply, {ok, Sum}} ->
	    ?assertEqual(40.27433388230814, Sum)
    after 3000 ->
	    exit(timeout)
    end,

    {ok, noreply} = server:stop().

sum_areas_unknown_shapes_test() ->
    {ok, noreply} = server:start(),
    {ok, _Pid} = client:start(),
    {ok, noreply} = client:connect(),
    receive after 1 -> ok end,
    Shapes = [{ellipse, 3, 4}],

    Reply = sum_areas(Shapes),

    ?assertMatch({reply, {error, {function_clause, _Detail}}}, Reply),

    {ok, noreply} = server:stop().

async_sum_areas_unknown_shapes_test() ->
    {ok, noreply} = server:start(),
    {ok, _Pid} = client:start(),
    {ok, noreply} = client:connect(),
    receive after 1 -> ok end,
    Shapes = [{ellipse, 3 ,4}],

    {ok, noreply} = async_sum_areas(Shapes),

    receive
	Reply ->
	    ?assertMatch({reply, {error, {function_clause, _Detail}}}, Reply)
    end,

    {ok, noreply} = server:stop().
