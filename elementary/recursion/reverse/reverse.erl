-module(reverse).
-export([reverse/1]).

reverse([]) -> [];
%reverse([H|T]) -> [H]++reverse(T).
%reverse([H|T]) -> reverse([T])++[H].
reverse([H|T]) -> reverse(T)++[H].
%reverse([H|T]) -> [H|reverse(T)].    
