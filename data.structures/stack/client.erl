-module(client).
-export([main/1, use_push/2, use_pop/1]).
-include("../stack/include/stack.hrl").
-include_lib("eunit/include/eunit.hrl").

use_pop_test_() ->
    ?_assertEqual({[], ok}, use_pop([1])).

use_push_test_() ->
    ?_assertEqual([9,0], use_push(9, [0])).

use_top_test_() ->
    ?_assertEqual({nil, ok}, use_top([])).

use_top_a_test() ->
    [
     {"When client invokes methods 'top' of '[a]', then Stack must return 'a'", ?assertEqual(a, use_top([a]))},
     {"When the client invokes method 'push' 'b' to '[a]', then Stack must return '[b,a]'", ?assertEqual([b,a], use_push(b, [a]))}
    ].

use_empty_test_() ->
    ?_assertEqual(true, use_empty([])).

use_empty_on_nempty_test_() ->
    ?_assertEqual(false, use_empty([a])).


%%% Now, only integers expected
%%% TODO capability consume different data types
main([E,L]) ->
    Elm = list_to_integer(atom_to_list(E)),
    Tokens = lists:map(fun (X) -> list_to_integer(X) end, string:tokens(atom_to_list(L), "[],")),
    io:format("~w~n", [client:use_push(Elm, Tokens)]).

use_pop(L) ->
    ?pop(L).

use_push(Elm, L) ->
    ?push(Elm, L).
    
use_top(L) ->
    ?top(L).

use_empty(L) ->
    ?empty(L).
