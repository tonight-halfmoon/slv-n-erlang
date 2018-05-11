-module(hhfuns2).
-compile(export_all).

%map(_, []) -> [];
%map(F, [H|T]) -> [F(H)|map(F,T)].

map([],_) -> [];
map([H|T], F) -> [map(T,F)|F(H)].
    
incr(A) -> A + 1.

same(K) -> K .
