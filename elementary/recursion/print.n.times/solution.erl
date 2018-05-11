-module(solution).
-export([main/0]).

main() ->
    {ok, [N]} = io:fread("", "~d"),
    io:fwrite("~w~n", [N]),
    print('Hello World', N),
	true.

print(_, 0) -> ok;
print(String, N) ->
     io:fwrite("~s~n", [String]),
     print(String, N-1).
