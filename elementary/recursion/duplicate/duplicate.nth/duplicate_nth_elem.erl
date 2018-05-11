-module(duplicate_nth_elem).
-export([dup/3]).
-include_lib("eunit/include/eunit.hrl").

dup(List, _, 0) ->
    List;
dup(List, Nth, Times) ->
    ?assert(is_integer(Nth)),
    ?assert(0=<Nth),
    ?assert(is_integer(Times)),
    ?assert(0 < Times),
    ?assert(is_list(List)),
    dup(List, Nth, Times, List).
dup(_, _, 0, Output) ->
    Output;
dup(List, Nth, Times, Output) ->
    dup(List, Nth, Times - 1, [nth(List,Nth)|Output]).

nth([], _) ->
    [];
nth([H|T], Nth) ->
    nth(T, Nth, H).
nth(_, 0, Elmnth) ->
    Elmnth;
nth(List, Nth, _) ->
    ?assert(length(List)>=Nth),
    [H|T] = List,
    nth(T, Nth - 1, H).
    
    
