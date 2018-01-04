-module(ait).
-export([start/0, stop/0]).


start() ->
    application:load(grsa),
    application:start(grsa).

stop() ->
    application:stop(grsa),
    application:unload(grsa).
