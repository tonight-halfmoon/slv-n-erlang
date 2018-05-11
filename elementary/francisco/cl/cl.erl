-module(cl).
-export([t/1, rt/1, rt2/1]).

t(N) ->
    t(N, []).

t(N, R) when N > 0->
    t(N - 1, [N|R]);
t(0, R) ->
    R.

rt(N) when N > 0 ->
    rt(N, 1, []).

rt(N, N, R) ->
    [N|R];
rt(N, I, R) ->
    rt(N, I + 1, [I|R]).

% or rt can be defined by reversing the final output of t/1.

rt2(N) ->
    %lists:reverse(t(N)).
    reverse(t(N)).

reverse(L) ->
    reverse(L, []).

reverse([H|T], R) ->
    reverse(T, [H|R]);
reverse([], R) ->
    R.
