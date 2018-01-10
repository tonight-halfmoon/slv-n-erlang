-module(genrs_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, Args) ->
    genrs_sup:start_link(Args).

stop(_State) ->
    ok.
