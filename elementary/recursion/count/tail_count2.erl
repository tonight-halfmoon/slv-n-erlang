-module(tail_count2).
-export([count/1]).

count(T) -> tail_count(T, 0).

tail_count([],Count)->
    Count;
tail_count([_|Rest], Count) ->
     tail_count(Rest, Count+1).
