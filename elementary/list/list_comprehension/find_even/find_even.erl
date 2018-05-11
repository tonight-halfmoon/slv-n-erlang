-module(find_even).
-export([find_even/1]).

find_even(List) ->
    [Nex  || Nex <- List, Nex rem 2 =:=0].
