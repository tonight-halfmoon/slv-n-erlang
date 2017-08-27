-module(msvs).
-include_lib("eunit/include/eunit.hrl").
-export([msvs/2]).

missing_chars_in_the_1st_list_test() ->
    ?assertEqual([a,b,c,d], msvs([a,b,c], [a,a,b,b,c,c,d])).

msvs_must_return_4_and_9_as_missing_in_the_1st_lst_test() ->
    ?assertEqual([4, 9], msvs([3,4,6,7], [3,9,4,4,6])).

msvs_nothin_is_missing_is_an_empty_lst_test() ->
    ?assertEqual([], msvs([], [])).

msvs_first_list_does_not_miss_anything_because_the_2nd_is_already_empty_test() ->
    ?assertEqual([], msvs([v], [])).

msvs_notalot_test() ->
    ?assertEqual([4,12,23,32,34,45,89,770], msvs([3,4,6,7,9,5,770,10,12,19,20,23,12,45,12,45,34,32,12,12,34], [3,9,4,4,6,23,12,45,12,45,34,32,12,23,12,45,12,770,45,34,32,12,89,770,12,34,5])).

msvs_what_is_missing_in_the_1st_list_test() ->
    ?assertEqual([a,b], msvs([], [a,b])).

msvs_must_not_miss_when_1st_lst_having_all_elems_2nd_lst_even_in_reverse_test() ->
    ?assertEqual([], msvs([b,a], [a,b])).

msvs(L1, L2) ->
    msvs(L1, L2, []).

msvs(_, [], L) ->
    L;
msvs([], Rest, L) ->
   lists:umerge(lists:sort(Rest), L);
msvs(L1, [H2|T2], L) ->
    case exists(H2,L1)  of
	false ->
	    msvs(L1, T2, add_ifnot_inorder(H2, L));
	true ->
	    msvs(delete_(H2,L1), T2, L)
    end.

exists(_, []) ->
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
