-module(sumn).
-export([sum/1]).

sum(N) when N == 0 -> 0;
sum(N) when N == 1 -> 1;
sum(N) when N > 1 -> N + sum(N -1).