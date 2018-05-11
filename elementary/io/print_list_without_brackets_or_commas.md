lists:foreach(fun(X) -> io:format("~p " ,[X]) end, L).
lists:foreach(fun(X) -> io:format("~w ", [X]) end, miss_num(L1,L2)),