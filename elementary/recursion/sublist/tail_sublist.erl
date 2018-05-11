-module(tail_sublist).
-export([sublist/2]).
%-import(reverse,[reverse/1]).
-import(lists, [reverse/1]).

sublist(T,N) -> reverse(tail_sublist(T,N,[])).

tail_sublist([],_,SBL)->
    SBL;
tail_sublist(_,0,SBL) -> SBL;
tail_sublist([H|T], N, SBL) when N > 0 ->
    tail_sublist(T, N-1, [H|SBL]). %[SBL++H]).
			       
