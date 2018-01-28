-module(tSAG).

-export([start/0, stop/0]).

start() ->
    genrs_riakc:start().

stop() ->
    application:unload('TSAG'),
    application:stop('TSAG').
