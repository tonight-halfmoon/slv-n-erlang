-module(stack).
-export([empty/1, push/2, pop/1, top/1]).
-include_lib("eunit/include/eunit.hrl").

push_test() ->
    ?assertEqual([1], push(1,[])).

pop_test() ->
    ?assertEqual([], pop([1])).

pop_empty_test_() ->
    [{"When 'pop' an empty stack, then must return {[], ok}.", ?_assertEqual({[], ok}, pop([]))}].

empty_test() ->
    ?assertEqual(true, empty([])).

top_of_empty_test_() ->
    ?_assertEqual({nil, ok}, top([])).

top_of_non_empty_test_() ->
    Stack = [a,b], 
    [
     {"When top is invoked on a non-empty stack '[a,b]', the stack should have the same elements afterwards.", ?_assertEqual(a, top(Stack))},
     {"When push 'c' is invoked after top on '[a,b]', then the stack must contain '[c,a,b]'", ?_assertEqual([c,a,b], push(c, Stack))}
    ].

empty([]) ->
    true;
empty(_) -> 
    false.

push(X, S)->
    [X|S].

pop([]) -> 
    {[], ok};
pop([_|T])->
    %{H,T}.
    T.

top([]) ->
    {nil, ok};
top([H|_]) ->
    H.
