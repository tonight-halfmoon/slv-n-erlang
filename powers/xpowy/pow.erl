-module(pow).
-export([int_pow/2]).

%% Bob Ippolito implemented functino 'int_pow' 
%% Reference of Source Origin
%% https://github.com/mochi/mochiweb/blob/master/src/mochinum.erl#L50
%% http://erlang.org/pipermail/erlang-questions/2012-March/065257.html
%% The function is copied and utilized here free of use according the terms of the source origin's license

int_pow(_X, 0) ->
    1;
int_pow(X, N) when N > 0 ->
    int_pow(X, N, 1).

int_pow(X, N, R) when N < 2 ->
    R * X;
int_pow(X, N, R) ->
    int_pow(X * X, N bsr 1, case N band 1 of 1 -> R * X; 0 -> R end).
