-module(amqp_sp).

-behaviour(application).

-export([start/0]).
-export([start/2, stop/1]).

start() ->
    {ok, _} = application:ensure_all_started(amqp_service_provider),
    ok.

start(_Type, DeclareArgs) ->
    amqp_sp_sup:start_link(DeclareArgs).

stop(_State) ->
    ok.
