-module(mnplt_n).
-compile(export_all).

mnp(0, _) -> 0;
mnp(N, F) ->  F(N).

sum(0) -> 0;
sum(1) -> 1;
sum(N) -> N + sum(N-1).

factorial(N) when N == 0 ->
    1;
factorial(N) when N == 1 ->
    1;
factorial(N) when N == 2->
    2;
factorial(N) when N > 2 ->
    N * factorial(N -1).
