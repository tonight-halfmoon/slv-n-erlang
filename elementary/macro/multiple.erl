-module(multiple).

-export([multiple/2]).
-export([multiple2/2]).
-export([multiple3/2]).
-export([multiple4/2]).

-define(MULTIPLE(X, Y), X rem Y == 0).

multiple(X, Y) ->
    ?MULTIPLE(X,Y).

%% Macro helps to reduce the following code if implemented otherwise:

%% Alt. 1
multiple2(X, Y) ->
    case X rem Y of
	0 ->
	    true;
	_ ->
	    false
    end.

%% Alt. 2
multiple3(X, Y) ->
    multiple(X, Y, X rem Y).

multiple(_X, _Y, 0) ->
    true;
multiple(_X, _Y, _) ->
    false.

%% Alt. 3
multiple4(X, Y) when X rem Y == 0 ->
    true;
multiple4(_X, _Y) ->
    false.
