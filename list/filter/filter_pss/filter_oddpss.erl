-module(filter_oddpss).
-include_lib("eunit/include/eunit.hrl").
-export([main/0, filter_oddpss/1]).

filter_postions_test() ->
    ?assertEqual([5,4,7,8], filter_oddpss([2,5,3,4,6,7,9,8])).

filter_oddposs_empty_test() ->
    ?assertEqual([], filter_oddpss([])).

filter_oddposs_one_test() ->
    ?assertEqual([], filter_oddpss([1])).

filter_oddposs_has2_test() ->
    ?assertEqual([2], filter_oddpss([1,2])).

filter_oddpss_3elems_test() ->
    ?assertEqual([5], filter_oddpss([2,5,3])).

filter_oddpss_4elems_test() ->
    ?assertEqual([5,6], filter_oddpss([2,5,3,6])).

main() ->
    Xs = read_list(),
    print(filter_oddpss(Xs)),
    true.

filter_oddpss(L)->
    filter_oddpss(L, []).

filter_oddpss([], L) ->
    L;
filter_oddpss([_], L) ->
    L;
filter_oddpss([_,Heven|T], L) ->
    filter_oddpss(T, lists:append([L, [Heven]])).

read_list() ->
    read_list(true, []).
read_list(true, L)->
    Line = io:get_line(""),
    if Line =:= eof ->
	    read_list(false, L);
       true ->
	    Tokens = string:tokens(string:strip(Line,right,$\n),"\n"),
	    if Tokens =:= [] ->
		    read_list(false, L);
	       Tokens =:= ["eof"] ->
		    read_list(false, L);
	       true ->
		    TokenInts = lists:map(fun erlang:list_to_integer/1, Tokens),
		    read_list(true, lists:append([L, TokenInts]))
	    end
    end;

read_list(false, L) ->
    L.

print([]) ->
    ok;
print([H|T] = L) when is_list(L) ->
    print(H),
    print(T);
print(X) ->
    io:fwrite("~w~n", [X]).
