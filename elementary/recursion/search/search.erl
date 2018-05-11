-module(search).
-export([search/2]).
-include_lib("eunit/include/eunit.hrl").

search_not_found_test()->
    Expected_value = not_found,
    L = [a,b,v,c,f,g],
    X = k,
    ?assertEqual(Expected_value, search(X,L)).
search_test()->
    Expected_value = ok,
    L = [a,b,c,d],
    X = a,
    ?assertEqual(Expected_value, search(X,L)).

search(_,[])->
    not_found;
search(X, [X|_]) ->
    ok;
search(X, [_|T]) ->
    search(X, T).

