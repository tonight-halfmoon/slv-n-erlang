%%%% timeout on test case #2 and #3

-module(solution).
-export([main/0]).
-include_lib("eunit/include/eunit.hrl").

miss_num_test() ->
    ?assertEqual([4,9], miss_num([3,4,6,7], [3,9,4,4,6])).
miss_num_empty_test() ->
    ?assertEqual([], miss_num([], [])).
miss_num_notalot_test() ->
    ?assertEqual([4,12,23,32,34,45,89,770], miss_num([3,4,6,7,9,5,770,10,12,19,20,23,12,45,12,45,34,32,12,12,34], [3,9,4,4,6,23,12,45,12,45,34,32,12,23,12,45,12,770,45,34,32,12,89,770,12,34,5])).

main() ->
    {ok, [N]} = io:fread("", "~d"),
    L1 = read_list(N),
    {ok, [N2]} = io:fread("", "~d"),
    L2 = read_list(N2),
    print_output(miss_num(L1,L2)),
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
	    miss_num(L1, T2, lists:umerge([H2],L));			      
	true ->
	    miss_num(delete_(H2,L1), T2, L)
    end.
    
add_ifnot_inorder(X, L) ->
    add_ifnot_inorder(X, L, []).
add_ifnot_inorder(X, [X|T], L) ->
    L ++ [X|T];
add_ifnot_inorder(X, [Y|T], L) when X =< Y ->
    L ++ [X] ++ [Y|T];
add_ifnot_inorder(X, [Y|T], L) when X>Y ->
    add_ifnot_inorder(X, T, L ++ [Y]);
add_ifnot_inorder(X, [], L) ->
    L ++ [X].


exists(_, [])->
    false;
exists(X, [X|_]) ->
    true;
exists(X, [_|T]) ->
    exists(X, T).

delete_(X, L)->
    delete_(X, L, []).

delete_(_, [], L) ->
    L;
delete_(X, [X|T], L) -> 
    L ++ T;
delete_(X, [H|T], L) -> 
    delete_(X, T, [H|L]).

read_list(N) ->
    read_list(N, 0, []).
read_list(0, _, L) ->
    L;
read_list(N, N, L) ->
    L;
read_list(N, I, L) -> 
    {ok, [X]} = io:fread("", "~d"),
    read_list(N, I+1, [X|L]).

print_output([]) ->
    ok;
print_output(L) when is_list(L) ->
    [H|T] = L,
    print_output(H),
    print_output(T);
print_output(X) ->
    io:fwrite("~w ", [X]).
