-module(solution).
-export([main/0]).

main() ->
    {ok, [N]} = io:fread("", "~d"),
    L = read(N, "~d"),
    display(sum_(L)),
    true.

sum_([]) ->
    0;
sum_([H|T]) ->
    sum_(T, H).
sum_([], S)->
    S;
sum_([H|T], S) ->
    sum_(T, S+H).


display([]) ->
    ok;
display(X) when not is_list(X) -> io:fwrite("~w~n", [X]);
display([H|T]) -> display(H), 
		  display(T).

read(0, _)-> [];
read(N, D) ->
    {ok, [X]} = io:fread("", D),
    [X|read(N-1, D)].
