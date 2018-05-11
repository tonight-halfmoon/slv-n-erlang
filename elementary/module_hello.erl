-module(module_hello).
-export([hello_world/0]).

hello_world() -> io:fwrite("hello, Erlang your door!").