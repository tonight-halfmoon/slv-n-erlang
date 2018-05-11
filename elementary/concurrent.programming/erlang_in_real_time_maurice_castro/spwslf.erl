-module(spwslf).
-export([start/0,newfn/0]).

start() ->
    MyPid = self(),
    io:format("demo: ~w~n", [MyPid]),
    NewPid = spawn(spwslf, newfn, []),
    io:format("demo: ~w~n", [NewPid]).

newfn()->
    MyPid = self(),
    io:format("newfn: ~w~n", [MyPid]).
