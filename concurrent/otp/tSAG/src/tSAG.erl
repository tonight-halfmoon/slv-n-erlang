-module(tSAG).

-export([start/0, stop/0]).

start() ->
    genrs_riakc:start().

stop() ->
    application:unload('tSAG'),
    application:stop('tSAG').