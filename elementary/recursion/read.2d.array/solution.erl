-module(solution).
-export([main/0]).

main() ->
    io:fwrite("~w~n", [read_2d_array(2,2, "~d")]),
    true.

read_array(0,_) ->
    [];
read_array(N,D) ->
    {ok, [X]} = io:fread("", D),
    [X|read_array(N-1,D)].

read_2d_array(0,_,_) ->
    [];
read_2d_array(N,M,D) ->
    Q = read_array(N,D),
    [Q|read_2d_array(N-1,M,D)].
