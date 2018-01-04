-module(grs_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, Args) ->
    grs_sup:start_link(Args).

stop(_State) ->
    ok.
