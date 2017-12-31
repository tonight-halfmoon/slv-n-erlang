-module(resas_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, Args) ->
    resa_sup:start_link(Args).

stop(_State) ->
    ok.
