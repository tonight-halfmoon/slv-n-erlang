-module(client_tests).
-include_lib("eunit/include/eunit.hrl").
-import(server, [start/0, stop/0]).
-import(client, [sum_areas/1, async_sum_areas/1]).

sum_areas_test() ->
    {ok, _Pid} = start(),
    Shapes = [{circle, 3}, {rectangle, 3, 4}],

    {reply, {ok, Sum}} = sum_areas(Shapes),

    ?assertEqual(40.27433388230814, Sum),

    {ok, noreply} = stop().

async_sum_areas_test() ->
    {ok, _Pid} = start(),
    Shapes = [{circle, 3}, {rectangle, 3, 4}],

    {ok, noreply} = async_sum_areas(Shapes),

    receive
	{reply, {ok, Sum}} ->
	    ?assertEqual(40.27433388230814, Sum)
    after 3000 ->
	    exit(timeout)
    end,

    {ok, noreply} = stop().

sum_areas_unknown_shapes_test() ->
    {ok, _Pid} = start(),
    Shapes = [{ellipse, 3, 4}],

    Reply = sum_areas(Shapes),

    ?assertMatch({reply, {error, {function_clause, _Detail}}}, Reply),

    {ok, noreply} = stop().

async_sum_areas_unknown_shapes_test() ->
    {ok, _Pid} = start(),
    Shapes = [{ellipse, 3 ,4}],

    {ok, noreply} = async_sum_areas(Shapes),

    receive
	Result ->
	    ?assertMatch({reply, {error, {function_clause, _Detail}}}, Result)
    end,

    {ok, noreply} = stop().
