-module(nivs).
-include_lib("eunit/include/eunit.hrl").
-export([search/2]).

search_not_found_test() ->
    ?assertEqual(not_found, search(k, [a,b,v,c,f,g])).

search_test()->
    ?assertEqual(found, search(a, [b,c,a,c,c])).

search_x_in_empty_list_wontbefound_test() ->
    ?assertEqual(not_found, search(a, [])).

search(_, []) ->
    not_found;
search(X, [X|_]) ->
    found;
search(X, [_|T]) ->
    search(X, T).
