-module(onload).
-export([onload/0]).
-on_load(onload/0).
-vsn(1.2).
-type mm() :: integer().
-type m() :: any().
-type n() :: none().
-type o() :: nonempty_maybe_improper_list().
-record(rec , {f1 :: mm()}).

onload() ->
    io:format("Hi! Thanks for calling.~n").
