-module(hhfuns).
-compile(export_all).

one() ->
    1.
two() ->
    2.

add(X,Y) ->
     X() + Y().

increment([])    -> [];
increment([H|T]) -> [H+1|increment(T)].

decrement([])    -> [];
decrement([H|T]) -> [H-1|decrement(T)].

%%See how similar these functinos are? They basically do the same thing: they cycly through a list, apply a function on each element (+ or -) and then call themselves again. There is almost nothing changing in that code: only the applied function and the recursive call are different. The core of a recursive call on a list like that is always the same. We'll abstract all the similar parts in a single function (map/2) that will take another function as an argument:

map(_, [])    -> [];
map(F, [H|T]) -> [F(H)|map(F,T)].

incr(X) -> X + 1.
decr(X) -> X - 1.




