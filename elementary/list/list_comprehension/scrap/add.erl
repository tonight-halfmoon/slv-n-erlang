-module(add).
-export([add/2]).

% silly solution!!
add(Set, List2add) ->
    Set ++ [Next || Next <- List2add]. % this only works if the first and second ops are list
