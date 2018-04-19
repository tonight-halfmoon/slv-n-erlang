16> F = fun(X) when is_float(X) -> io:format("Found a float number: ~.2f~n", [X]); (X) -> io:format("~p~n", [X]) end.

