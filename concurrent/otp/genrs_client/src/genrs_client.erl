-module(genrs_client).

-behaviour(application).

-export([start/0]).
-export([start/2, stop/1]).

-include_lib("genrs/include/amqp_connect.hrl").

start() ->
    application:ensure_all_started(amqp_service_provider),
    {ok, _} = application:ensure_all_started(genrs_client),
    ok.

start(_Type, _Args) ->
    genrs_client_sup:start_link(#amqp_connect_args{exch=?exch, queue=?queue}).

stop(_State) ->
    ok.
