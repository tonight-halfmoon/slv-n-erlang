-module(swap).
-export([swap_adjacent/1]).

swap_adjacent([]) ->
    [];
swap_adjacent([A]) -> % to be able to consider non-even lists
    [A];
swap_adjacent([H,Next|T]) ->
    [Next,H] ++
    swap_adjacent(T).
    
