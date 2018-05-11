-module(solution2).
-export([main/0]).

main () ->
    L = read(),
    %io:format("Complete input: ~w~n", [L]),
    %display(reverse(L)).
    display(L).

reverse([]) -> [];
reverse(X) when not is_list(X) -> X;
reverse([H|T]) -> reverse(T, H).
reverse([H|T], L) -> reverse(T, [H|L]);
reverse([], L) -> L.

display([]) -> ok;
display(X) when not is_list(X)->
        io:fwrite("~w~n", [X]);
display(PL) when is_list(PL) ->
       [H|T] = PL,
       display(H),
       display(T).

read() ->
    read(1, []).
read(0, L) -> L;
read(_, L) ->
    Line = io:get_line(""),
    if Line =:= eof ->
	    read(0, L);
       true ->
	    %io:format("Line is: ~p~n", [Line]),
	    %Token  = lists:map(fun erlang:list_to_integer/1, string:tokens(string:strip(Line, right, $\n), "\n")),
	    Token =  string:tokens(string:strip(Line, right, $\n), "\n"),
	    %io:format("Token is: ~p~n", [Token]),
	    if Token =:= eof ->
		    read(0, L);
	       Token =:= ["eof"] ->
		    read(0, L);
	    	Token =:= [] ->
		    read(0, L);
	       Token =/= [] ->
		    TokenInts = lists:map(fun erlang:list_to_integer/1, Token),
		    read(1, [TokenInts|L])
	    end
    end.
