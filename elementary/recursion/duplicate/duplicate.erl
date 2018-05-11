-module(duplicate).
-export([duplicate/2]).

duplicate(0,_) -> [];
duplicate(N, T) when N > 0 -> [T|duplicate(N-1,T)].