-module(pingpong).
-export([run/0]).

run() ->
    Pid = spawn(fun ping/0),
    
