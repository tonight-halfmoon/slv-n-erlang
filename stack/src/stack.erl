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
    [
     {"When top is invoked on '[a,b]', then it must return 'a'", ?_assertEqual(a, top([a,b]))},
     {"When push 'c' is invoked on '[a,b]', then it must reutnr '[c,a,b]'", ?_assertEqual([c,a,b], push(c, [a,b]))},
     {"When pop is invoked on '[t,u,i]', then the stack must contain only '[u,i]'", ?_assertEqual([u,i], pop([t,u,i]))},
     {"And when pop is invoked on '[6,4]', then the stack must contain only '[4]'", ?_assertEqual([4], pop([6,4]))}
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
