-module(reverse).
-export([reverse/1]).

%% version 1
%reverse ([]) ->
%    [];
%reverse([H|T])->
%    reverse(T)++[H].


%% version 2
%reverse (L) ->
%    reverse(L, []).
%reverse([], Rvrsd) ->
%    Rvrsd;
%reverse([H|T], Rvrsd) ->
%    reverse(Rvrsd, reverse(T)++[H]).


%% version 3

reverse([]) ->
    [];
reverse(L) ->
    reverse(L, []).

reverse([H|T], L) ->
    reverse(T, [H] ++ L); %% or reverse(T, [H|L]);
reverse([], L) ->
    L.

