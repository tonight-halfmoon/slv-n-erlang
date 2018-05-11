-module(sumn_tail).
-export([tail_sum/1]).

tail_sum(N) -> tail_sum(N, 0).

tail_sum(0, Acc) -> Acc;
tail_sum(N, Acc) when N > 0 -> tail_sum(N -1, N + Acc). 