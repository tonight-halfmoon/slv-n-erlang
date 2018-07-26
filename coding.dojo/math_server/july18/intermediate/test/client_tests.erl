-module(client_tests).
-include_lib("eunit/include/eunit.hrl").
-import(server, [start/0, stop/1]).
-import(client, [sum_areas/2, async_sum_areas/2]).

sum_areas_test() ->
    {ok, Pid} = start(),
    Shapes = [{circle, 3}, {rectangle, 3, 4}],

    {reply, {ok, Sum}} = sum_areas(Shapes, Pid),

    ?assertEqual(40.27433388230814, Sum),

    {ok, noreply} = stop(Pid).

async_sum_areas_test() ->
    {ok, Pid} = start(),
    Shapes = [{circle, 3}, {rectangle, 3, 4}],

    {ok, noreply} = async_sum_areas(Shapes, Pid),

    receive
	{reply, {ok, Sum}} ->
	    ?assertEqual(40.27433388230814, Sum)
    after 3000 ->
	    exit(timeout)
    end,

    {ok, noreply} = stop(Pid).

sum_areas_unknown_shapes_test() ->
    {ok, Pid} = start(),
    Shapes = [{ellipse, 3, 4}],

    Reply = sum_areas(Shapes, Pid),

    ?assertMatch({reply, {error, {function_clause, _Detail}}}, Reply),

    {ok, noreply} = stop(Pid).

async_sum_areas_unknown_shapes_test() ->
    {ok, Pid} = start(),
    Shapes = [{ellipse, 3 ,4}],

    {ok, noreply} = async_sum_areas(Shapes, Pid),

    receive
	Result ->
	    ?assertMatch({reply, {error, {function_clause, _Detail}}}, Result)
    end,

    {ok, noreply} = stop(Pid).
