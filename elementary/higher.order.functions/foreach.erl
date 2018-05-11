-module(foreach).
-compile(export_all).

foreach(_, []) ->
    ok;
foreach(F, [H|T]) ->
    F(H),
    foreach(F, T).


main() ->
    foreach(fun(H) -> io:format("~p~n", [H]) end, [1,2,3,4,5,6]),
    true.
