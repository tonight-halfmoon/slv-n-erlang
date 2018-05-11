-module(quicksort).
-export([main/0]).

main() ->
    {ok, [N]} = io:fread("", "~d"),
    {ok, Ar} = io:fread("", string:join(replicate(N, "~d"), " ")),
    Ar_out = sort(Ar),
    io:fwrite(string:concat(string:join(replicate(N,"~w"), " "), "\n"),Ar_out)
    .
replicate(T, _) when T =< 0 ->
    [];
replicate(T, O) -> [O|replicate(T-1, O)].

sort([Pivot|T]) ->
    L = lists:filter(fun(X) -> X < Pivot end, T),
    R = lists:filter(fun(X) -> X > Pivot end, T),
    lists:append([L, [Pivot], R]).
    
