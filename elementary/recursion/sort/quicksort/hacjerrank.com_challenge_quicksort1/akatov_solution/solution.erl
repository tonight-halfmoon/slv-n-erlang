-module(solution).
-export([main/1]).
-export([main/0]).

main() -> main(ok).

main(_) ->
  {ok, [N]} = io:fread("", "~d"),
  {ok, Arr} = io:fread("", string:join(replicate(N, "~d"), " ")),
  Ar = sort(Arr),
  io:fwrite(string:concat(string:join(replicate(N, "~w"), " "), "\n"), Ar).

replicate(T, _) when T =< 0 -> [];
replicate(T, O) -> [ O | replicate(T-1, O)].

sort([Pivot|Arr]) ->
  L = lists:filter(fun(X) -> X < Pivot end, Arr),
  R = lists:filter(fun(X) -> X > Pivot end, Arr),
  lists:append([L, [Pivot], R]).
