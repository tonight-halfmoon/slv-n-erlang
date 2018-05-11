%%
%  12th April 16
%%
-module(integerperms).
-export([main/0]).
-include_lib("eunit/include/eunit.hrl").

intperms_test() ->
    ?assertEqual([123,132,213,231,312,321], intperms(123)).

main() ->
    {ok, [Int]} = io:fread("", "~d"),
    io:fwrite("Output: ~w~n",[intperms(Int)]),
    true.

intperms(N) ->
    Digits  = digits(N),
    intperms(Digits,[], length(Digits), 0, []).

intperms([],_, _, _, _) ->
    true;
intperms(_,Xs, M,M, TentPerms) ->
    io:fwrite("Xs: ~w~n", [Xs]),
    io:fwrite("TentPerms: ~w~n", [TentPerms]),
    reverse(zip_(Xs,TentPerms));

%% This is a base case for the outer formula
%intperms([F,L], Xs, _, _, Perms)->
%    intpermsXs(Xs,[[F,L],[L,F]|Perms]);

intperms(Digits, Xs,M, Nex, Perms) ->
    X = lists:nth(Nex+1, Digits),
    Rest = lists:subtract(Digits, [X]),
    intperms(Digits, [X|Xs], M, Nex+1, [Rest|Perms]).

digits(N) when N>=0 andalso N=<9 -> [N];
digits(N) -> digits(N, []).
digits(0, Ds)-> Ds;
digits(N, Ds) -> digits(trunc(N/10), [trunc(N rem 10)|Ds]).

zip_(Xs, Tents) -> zip_(Xs, Tents, []).
zip_([],_, Perms) -> Perms;
zip_([X|Xs], [H|T], Perms) -> zip_(Xs, T, [[X|H]|Perms]).

reverse(S) -> reverse(S, []).
reverse([], R) -> R; 
reverse([H|T], R) -> reverse(T, [H|R]). 
    
