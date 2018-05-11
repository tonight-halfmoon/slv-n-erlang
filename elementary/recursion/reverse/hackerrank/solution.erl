-module(solution).
-export([main/0]).

main() ->	
    Input = io:get_line(""),
    L = lists:map(fun erlang:list_to_integer/1, string:tokens(string:strip(Input, right, $\n), " ")),
    {ok, [L]} = io:fread("", "~d"),
    case check_constraints(L) of 
	    true->
	       display(reverse(L))
	   false ->
	       ok
   end.
.
reverse([]) -> [];
reverse(X) -> X;
reverse([H|T]) -> reverse(T, H).
reverse([H|T], L) -> reverse(T, [H|L]);
reverse([], L) -> L.

display([]) -> ok;
display(X) when is_integer(X) ->
        io:fwrite("~w~n", [X]);
display(PL) when is_list(PL) -> 
        [H|T] = PL,
        display(H),
        display(T). 


check_constraints(L)->
    case grte(length(L),1) andalso lse(length(L), 100) of
	true->
	    check_elem_constraints(L);
	false ->
	    false
    end.

check_elem_constraints([]) -> true;
check_elem_constraints([H|T]) ->
    case grte(H,0) andalso lse(H,100) of
		true ->
		    check_elem_constraints(T);
		false ->
		     false
	    end.

grte(X,Y) -> X >= Y.
lse(X,Y) -> X =< Y.

