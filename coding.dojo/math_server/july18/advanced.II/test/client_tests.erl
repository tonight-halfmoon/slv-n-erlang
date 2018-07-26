-module(client_tests).
-include_lib("eunit/include/eunit.hrl").
-include("client.hrl").

start_test() ->
    {ok, Pid} = client:start(),

    ?assertEqual(Pid, whereis(?Client)),
    ?assert(is_process_alive(whereis(?Client))),

    {ok, noreply} = client:stop().

stop_test() ->
    {ok, Pid} = client:start(),
    ?assertEqual(Pid, whereis(?Client)),

    {ok, noreply} = client:stop(),
    receive after 1 -> ok end,

    ?assertNot(is_process_alive(Pid)),   
    ?assertEqual(undefined, (whereis(?Client))).

when_server_get_down_client_terminate_test() ->
    {ok, noreply} = server:start(),
    {ok, Pid} = client:start(),
    receive after 1 ->
		    ok end,
    {ok, noreply} = server:stop(),
    receive after 3 ->
		    ok end,

    ?assertNot(is_process_alive(Pid)).

sum_areas_test() ->
    {ok, noreply} = server:start(),
    Shapes = [{circle, 3}, {rectangle, 3, 4}],
    {ok, _Pid} = client:start(),

    {reply, {ok, Sum}} = client:sum_areas(Shapes),

    ?assertEqual(40.27433388230814, Sum),

    {ok, noreply} = client:stop(),
    {ok, noreply} = server:stop().

async_sum_areas_test() ->
    {ok, noreply} = server:start(),
    Shapes = [{circle, 3}, {rectangle, 3, 4}],
    {ok, _Pid} = client:start(),

    {ok, noreply} = client:async_sum_areas(Shapes),

    receive
	{reply, {ok, Sum}} ->
	    ?assertEqual(40.27433388230814, Sum)
    after 3000 ->
	    exit(timeout)
    end,

    {ok, noreply} = server:stop().

sum_areas_unknown_shapes_test() ->
    {ok, noreply} = server:start(),
    Shapes = [{ellipse, 3, 4}],
    {ok, _Pid} = client:start(),

    Reply = client:sum_areas(Shapes),

    ?assertMatch({reply, {error, {function_clause, _Detail}}}, Reply),

    {ok, noreply} = server:stop().

async_sum_areas_unknown_shapes_test() ->
    {ok, noreply} = server:start(),
    Shapes = [{ellipse, 3 ,4}],
    {ok, _Pid} = client:start(),

    {ok, noreply} = client:async_sum_areas(Shapes),

    receive
	Result ->
	    ?assertMatch({reply, {error, {function_clause, _Detail}}}, Result)
    end,

    {ok, noreply} = server:stop().
