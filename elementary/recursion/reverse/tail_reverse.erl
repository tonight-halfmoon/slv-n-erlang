-module(tail_reverse).
-export([tail_reverse/1]).

tail_reverse(T) -> tail_reverse(T,[]).

tail_reverse([],RVRSD) -> RVRSD;
tail_reverse([H|T], RVRSD) ->
    tail_reverse(T,[H|RVRSD]).

