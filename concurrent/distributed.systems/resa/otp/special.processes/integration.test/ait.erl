-module(ait).
-export([start/0, stop/0]).


start() ->
    application:load(rssp),
    application:start(rssp).

stop() ->
    application:stop(rssp),
    application:unload(rssp).
