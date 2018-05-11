-module(tail_count).
-export([cnt/1]).

cnt(List) ->
    cnt(List, 0).

cnt([], Acc) ->
    Acc;
cnt([_|T], Acc) ->
    cnt(T, Acc + 1).
