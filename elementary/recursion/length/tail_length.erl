-module(tail_length).
-export([tail_len/1]).

tail_len(T) -> tail_len(T,0).

tail_len([], Acc) -> Acc;
tail_len([_|T], Acc) -> tail_len(T,1+Acc).


%len([]) -> 0;
%len([_|T]) -> 1 + len(T).
