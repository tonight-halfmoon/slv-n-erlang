-module(mcs).

-export([run/0]).

-export([spawn_mcs/2, stop_genrs/0]).

-include("config.hrl").

-include_lib("eunit/include/eunit.hrl").

run() ->
    start_genrs(),
    spawn_mcs(50, fun ?MODULE:stop_genrs/0).

spawn_mcs(Limit, Handle_stop) ->
    spawn_mcs(Limit, 1, Handle_stop).

spawn_mcs(Limit, Limit, Handle_stop) ->
    receive
    after 5000 ->
	    io:format("~n", []),
	    Handle_stop()
    end,
    ?assertEqual(undefined, whereis(?server)),
    done;
spawn_mcs(Limit, I, _fun)->
    Pid = spawn(simuc, attempt_dstats, []),
    io:format(">> next Pid ~p~n", [Pid]),
    spawn_mcs(Limit, I + 1, _fun).

start_genrs() ->
    application:load(genrs),
    application:start(genrs).

stop_genrs() ->
    application:stop(genrs),
    application:unload(genrs).
