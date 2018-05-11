-module(solution).
-export([main/0]).

main() ->
    {ok, _} = io:fread("", "~d"),
    Data = io:get_line(""),
    Num = lists:map(fun erlang:list_to_integer/1, string:tokens(Data," ")),
    io:fwrite("~B", [lists:sum(Num)]),
    true.
    
