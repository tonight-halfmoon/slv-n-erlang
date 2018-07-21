-module(client_tests).
-include_lib("eunit/include/eunit.hrl").
-import(server, [start/0, stop/0]).
-import(client, [sum_areas/1, async_sum_areas/1]).
-import(client, [connect/0, disconnect/0]).
-include("client.hrl").

connect_test() ->
    {ok, Pid} = connect(),

    ?assertEqual(Pid, whereis(?Client)),
    ?assert(is_process_alive(whereis(?Client))),

    {ok, noreply} = disconnect().

disconnect_test() ->
    {ok, Pid} = connect(),

    {ok, noreply} = disconnect(),
    receive after 1 -> ok end,

    ?assertNot(is_process_alive(Pid)),   
    ?assertEqual(undefined, (whereis(?Client))).

sum_areas_test() ->
    {ok, noreply} = start(),
    Shapes = [{circle, 3}, {rectangle, 3, 4}],
    {ok, _Pid} = connect(),

    {reply, {ok, Sum}} = sum_areas(Shapes),

    ?assertEqual(40.27433388230814, Sum),

    {ok, noreply} = stop().

async_sum_areas_test() ->
    {ok, noreply} = start(),
    Shapes = [{circle, 3}, {rectangle, 3, 4}],
    {ok, _Pid} = connect(),

    {ok, noreply} = async_sum_areas(Shapes),

    receive
	{reply, {ok, Sum}} ->
	    ?assertEqual(40.27433388230814, Sum)
    after 3000 ->
	    exit(timeout)
    end,

    {ok, noreply} = stop().

sum_areas_unknown_shapes_test() ->
    {ok, noreply} = start(),
    Shapes = [{ellipse, 3, 4}],
    {ok, _Pid} = connect(),

    M = sum_areas(Shapes),

    ?assertMatch({reply, {error, {function_clause, _Detail}}}, M),

    {ok, noreply} = stop().

async_sum_areas_unknown_shapes_test() ->
    {ok, noreply} = start(),
    Shapes = [{ellipse, 3 ,4}],
    {ok, _Pid} = connect(),

    {ok, noreply} = async_sum_areas(Shapes),

    receive
	M ->
	    ?assertMatch({reply, {error, {function_clause, _Detail}}}, M)
    end,

    {ok, noreply} = stop().

