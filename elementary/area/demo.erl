-module(demo).
-export([double/1, g_times/2]).
-date("April 2018").
-author("Ahmad Elghafari").
-vsn('12.0.20').

% This is a comment.
% Everything on a line after % is ignored.

double(Value) ->
    ?MODULE:g_times(Value, 2). % but we cannot call ?MODULE:times(Value, 2) inside the ?MODULE.

times(X, Y) ->
    X * Y.

g_times(X,Y) ->
    times(X,Y).
