-module(tail_duplicate).
-export([tail_duplicate/2]).

%tail_duplicate(N,T) -> tail_duplicate(N,T,0).
tail_duplicate(N,T) -> tail_duplicate(N,T,[]).

%tail_duplicate(0,_,Acc) -> Acc;
tail_duplicate(0,_,List) -> List;
%tail_duplicate(N,T,Acc) when N > 0 -> [T|tail_duplicate(N-1,T,1+Acc)].
tail_duplicate(N,T,List) when N > 0 -> tail_duplicate(N-1,T,[T|List]).


