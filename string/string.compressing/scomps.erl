-module(scomps).
-include_lib("eunit/include/eunit.hrl").
-export([compress/1]).

compress_empty_test() ->
    ?assertEqual([], compress([])).

compress_sol_test() ->
    ?assertEqual(a, compress([a])).

compress_two_test() ->
    ?assertEqual(a2, compress([a,a])).

compress_a3_test() ->
    ?assertEqual(a3b, compress([a,a,a,b])).

compress_aaabbb_must_yield_in_a3b3_test() ->
    ?assertEqual(a3b3, compress([a,a,a,b,b,b])).

compress_aaab1_test() ->
    ?assertEqual(a3b1, compress([a,a,a,b,1])).

compress([]) ->
    [];
compress([H|T]) ->
    compress(H, T, 1, []).

compress(Curr, [Curr|T], Occ, MSG) ->
    compress(Curr, T, Occ+1, MSG);
compress(Curr, [Next|T], 1, MSG) ->   
    compress(Next, T, 1, [to_list(Curr)|MSG]);
compress(Curr, [Next|T], Occ, MSG) ->
    compress(Next, T, 1, [integer_to_list(Occ), to_list(Curr)|MSG]);
compress(Last, [], 1, MSG) ->
    list_to_atom(string:join(reverse([to_list(Last)|MSG]), ""));
compress(Last, [], Occ, MSG)->
    list_to_atom(string:join(reverse([integer_to_list(Occ), to_list(Last)|MSG]), "")).

to_list(X) when is_integer(X) ->
    integer_to_list(X);
to_list(X) when is_atom(X) ->
    atom_to_list(X);
to_list(X) ->
    X.

reverse([])->
    [];
reverse([H|T]) ->
    reverse(T, [H]).
reverse([H|T], L)->
    reverse(T, [H|L]);
reverse([], L) ->
    L.

