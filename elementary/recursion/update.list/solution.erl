-module(solution).
-export([main/0]).
-include_lib("eunit/include/eunit.hrl").

update_list_abs_test() ->
    ?assertEqual([2,5,10], update_list_abs([-2,5,-10])).

update_list_abs_empty_test() ->
    ?assertEqual([], update_list_abs([])).

main() ->
    Xs = read_list(),
    write_output(update_list_abs(Xs)),
    true.

update_list_abs(L) ->
    update_list(fun abs/1, L, []).
update_list(Fun, [H|T], L) ->
    update_list(Fun, T, lists:append([L, [Fun(H)]]));
update_list(_, [], L) ->
    L.

abs(X) when X < 0 ->
    X * -1;
abs(X) ->
    X.

read_list() ->
    read_list(true, []).

read_list(false, L) ->
    L;
read_list(true, L) ->
    Line = io:get_line(""),
    if Line =:= eof ->
	    read_list(false, L);
       true ->
	    Tokens = string:tokens(string:strip(Line,right,$\n), "\n"),
	    if Tokens =:= [] ->
		    read_list(false, L);
	       true ->
		    TokenInts = lists:map(fun erlang:list_to_integer/1, Tokens),
		    read_list(true, lists:append([L, TokenInts]))
	    end
    end.

write_output([]) ->
    ok;
write_output(L) when is_list(L) ->
    [H|T] = L,
    write_output(H),
    write_output(T);
write_output(X) ->
    io:fwrite("~w~n", [X]).
