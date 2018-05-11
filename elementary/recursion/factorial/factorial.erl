-module(factorial).
-export([fac/1]).

%fac(N) when N == 0 -> 1;
%fac(N) when N > 0 -> N * fac(N - 1).

% with the help of pattern matching we can shorten the definition a bit:
fac(0) -> 1;
fac(N) when N > 0 -> N*fac(N-1).
