-module(extecho).
-export([start/0]).

start() ->
    register(echo, spawn(echo, loop, [])).
