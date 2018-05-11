-module(duplicate_first).
-export([dup/2]).
-include_lib("eunit/include/eunit.hrl").

dup(List, 0) ->
    List;
dup(List, Times) ->
    ?assert(is_integer(Times)),
    ?assert(0 < Times),
    ?assert(is_list(List)),
    dup(List, Times, List).

dup(_, 0, Output) ->
    Output;
dup(List, Times, Output) ->
    [H|_] = List,
    dup(List , Times-1, [H| Output]).

