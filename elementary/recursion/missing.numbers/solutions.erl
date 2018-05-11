-module(solutions).
-export([main/0]).
-include_lib("eunit/include/eunit.hrl").

miss_num_test() ->
    ?assertEqual([4,9], miss_num([3,4,6,7], [3,9,4,4,6])).

main() ->
    {ok, _} = io:fread("", "~d"),
    L1 = read_list2(),
    {ok, _} = io:fread("", "~d"),
    L2 = read_list2(),
    lists:foreach(fun(X) -> io:format("~p ", [X]) end, miss_num(L1,L2)),
    true.

miss_num(L1, L2)->
    miss_num(L1, L2, []).
miss_num(_, [], L) ->
    L;
miss_num([], _, L) ->
    L;
miss_num(L1, [H2|T2], L) ->
    case exists(H2,L1)  of
	false ->
	    miss_num(L1, T2, add_ifnot_inorder(H2,L));			      
	true ->
	    miss_num(delete_(H2,L1), T2, L)
    end.

exists(_, [])->
    false;
exists(X, [X|_]) ->
    true;
exists(X, [_|T]) ->
    exists(X, T).

delete_(X, L) ->
    delete_(X, L, []).
delete_(X, [X|T], L) -> 
    L ++ T;
delete_(X, [H|T], L) -> 
    delete_(X, T, [H|L]);
delete_(_, [], L) -> 
    L.

add_ifnot_inorder(X, L) ->
    add_ifnot_inorder(X, L, []).
add_ifnot_inorder(X, [X|T], L) ->
    L ++ [X|T];
add_ifnot_inorder(X, [Y|T], L) when X =< Y->
    L ++ [X,Y|T];
add_ifnot_inorder(X, [Y|T], L) when X > Y ->
    add_ifnot_inorder(X, T, L ++ [Y]);
add_ifnot_inorder(X, [], L) ->
    L ++ [X].

read_list2() ->
    case io:get_line("") of
	eof ->
            [];
	Line ->
	    lists:map(fun(X) -> list_to_integer(X) end, string:tokens(string:strip(Line, right, $\n), " "))
    end.
