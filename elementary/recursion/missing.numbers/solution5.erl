-module(solution5).
-include_lib("eunit/include/eunit.hrl").
-export([main/0]).

miss_chars_test() ->
    ?assertEqual([a,b,c,d], miss_num([a,b,c], [a,a,b,b,c,c,d])).

miss_num_test() ->
    ?assertEqual([4, 9], miss_num([3,4,6,7], [3,9,4,4,6])).

miss_num_empty_test() ->
    ?assertEqual([], miss_num([], [])).

miss_num_notalot_test() ->
    ?assertEqual([4,12,23,32,34,45,89,770], miss_num([3,4,6,7,9,5,770,10,12,19,20,23,12,45,12,45,34,32,12,12,34], [3,9,4,4,6,23,12,45,12,45,34,32,12,23,12,45,12,770,45,34,32,12,89,770,12,34,5])).



main() ->
    {ok, [N]} = io:fread("", "~d"),
    L1 = read_list(N),
    {ok, [N2]} = io:fread("", "~d"),
    L2 = read_list(N2),
    lists:foreach(fun(X) -> io:format("~w ", [X]) end, miss_num(L1,L2)),
    true.

miss_num(L1, L2)->
    miss_num(L1, L2, []).
miss_num(_, [], L) ->
    L;
miss_num([], Rest, L) ->
   lists:umerge(lists:sort(Rest), L); %without this instruction; the code has a bug with Test Case #0 (hackerrank)
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
add_ifnot_inorder(X, [Y|T], L) when X =< Y ->
    L ++ [X,Y|T];
add_ifnot_inorder(X, [Y|T], L) when X > Y ->
    add_ifnot_inorder(X, T, L ++ [Y]);
add_ifnot_inorder(X, [], L) ->
    L ++ [X].

read_list(_) ->
    case io:get_line("") of 
        eof ->
            []; 
	Line ->
	    lists:map(fun erlang:list_to_integer/1, string:tokens(Line, "\r\n\t "))
    end.
