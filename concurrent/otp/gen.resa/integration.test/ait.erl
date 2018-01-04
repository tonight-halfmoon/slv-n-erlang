-module(ait).
-export([start/0, stop/0]).


start() ->
    application:load(genrs),
    application:start(genrs).

stop() ->
    application:stop(genrs),
    application:unload(genrs).
