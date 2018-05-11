-module(list_gen).
-export([ml/1, rml/1, rml2/1]).

ml(N) ->
    ml(N, []).

ml(N, R) when N > 0 ->
    ml(N - 1, [N|R]);
ml(0, R) ->
    R.

rml(N) when N > 0 ->
    rml(N, 1, []).

rml( N, N, R) ->
    [N|R];
rml(N, I, R) ->
    rml(N, I + 1, [I|R]).

rml2(N) ->
    reverse(ml(N)).

reverse(L) ->
    reverse(L, []).

reverse([H|T], R) ->
    reverse(T, [H|R]);
reverse([], R) ->
    R.
