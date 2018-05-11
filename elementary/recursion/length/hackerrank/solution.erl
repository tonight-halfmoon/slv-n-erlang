-module(solution).
-export([main/0]).
-include_lib("eunit/include/eunit.hrl").

len_test() ->
    ?assertEqual(10, len([6,7,8,9,5,4,3,6,7,8])).

main() ->
    Xs = read_list(),
    print(len(Xs)),
    true.

len(L) ->
    len(L, 0).
len([], Len) ->
    Len;
len([_,_|T], Len) ->
    len(T, Len+2);
len([_|T], Len) ->
    len(T, Len+1).


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
		    TokenInts = lists:map(fun erlang:list_to_integer/1, Tokens ),
		    read_list(true, lists:append([L, TokenInts]))
	    end
    end.

print([]) ->
    ok;
print(L) when is_list(L) ->
    [H|T] = L,
    print(H),
    print(T);
print(X) ->
    io:fwrite("~w~n", [X]).
