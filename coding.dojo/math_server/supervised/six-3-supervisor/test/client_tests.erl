-module(client_tests).
-include_lib("eunit/include/eunit.hrl").
-include("client.hrl").
-include("minimal_supervisor.hrl").
-include("server.hrl").
-import(client, [sum_areas/1, async_sum_areas/1]).


sum_areas_test() ->
    ChildSpecList = [{transient, {server, start_link, []}}],
    minimal_supervisor:start_link(ChildSpecList),
    receive after 3 -> ok end,
    client:start(),
    client:connect(),
    receive after 3 -> ok end,
    Shapes = [{circle, 3}, {rectangle, 3, 4}],

    {reply, ?Server, {ok, Sum}} = sum_areas(Shapes),

    ?assertEqual(40.27433388230814, Sum),

    client:disconnect(),
    client:stop(),
    minimal_supervisor:stop().

async_sum_areas_test() ->
    ChildSpecList = [{transient, {server, start_link, []}}],
    minimal_supervisor:start_link(ChildSpecList),
    receive after 3 -> ok end,
    client:start(),
    client:connect(),
    receive after 3 -> ok end,
    Shapes = [{circle, 3}, {rectangle, 3, 4}],

    {ok, noreply} = async_sum_areas(Shapes),

    receive
	{reply, ?Server, {ok, Sum}} ->
	    ?assertEqual(40.27433388230814, Sum)
    after 100 ->
	    exit(timeout)
    end,

    client:disconnect(),
    client:stop(),
    minimal_supervisor:stop().

sum_areas_unknown_shapes_test() ->
    ChildSpecList = [{trasnient, {server, start_link, []}}],
    minimal_supervisor:start_link(ChildSpecList),
    receive after 3 -> ok end,
    client:start(),
    client:connect(),
    receive after 3 -> ok end,
    Shapes = [{ellipse, 3, 4}],

    Reply = sum_areas(Shapes),

    ?assertMatch({reply, ?Server,
 {error, {function_clause, _Detail}}}, Reply),

    client:disconnect(),
    client:stop(),
    minimal_supervisor:stop().

async_sum_areas_unknown_shapes_test() ->
    ChildSpecList = [{transient, {server, start_link, []}}],
    minimal_supervisor:start_link(ChildSpecList),
    receive after 3 -> ok end,
    client:start(),
    client:connect(),
    receive after 3 -> ok end,
    Shapes = [{ellipse, 3 ,4}],

    {ok, noreply} = async_sum_areas(Shapes),

    receive
	Reply ->
	    ?assertMatch({reply, ?Server, {error, {function_clause, _Detail}}}, Reply)
    after 200 ->
	    exit(timeout)
    end,

    client:disconnect(),
    client:stop(),
    minimal_supervisor:stop().
