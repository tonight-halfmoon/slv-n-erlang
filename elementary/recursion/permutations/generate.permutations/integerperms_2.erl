-module(integerperms_2).
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
    process(Digits, length(Digits) - 1).

    %intperms(Digits,[], length(Digits), 0, []).

process(Digits, MaxLen) ->
    process(Digits, Digits, MaxLen, []).

process(_, [A,B], _, Perms)-> [[A,B,Perms], [B,A,Perms]];
process(_, [A], _, Perms) -> [A|Perms];
process(_, Digits, MaxLen, Perms) ->
    {RRest, PermsNth} = intperms(Rest, Digitis, length(Digits), Len),
    process(StartIndex, Len+1, MaxRange, Digits, RRest, [PermsNth|Perms]).

intperms(Digits, StartIndex, M,  Len) ->
    intperms(Digits, [], StartIndex, M, Len, []).

intperms([],_, _, _,_, _) -> true;
intperms(_,Xs, M,M, _, TentPerms) ->
    io:fwrite("Xs: ~w~n", [Xs]),
    io:fwrite("TentPerms: ~w~n", [TentPerms]),
    reverse(zip_(Xs, TentPerms));

%% This is a base case for the outer formula
%intperms([F,L], Xs, _, _, Perms)->
%    intpermsXs(Xs,[[F,L],[L,F]|Perms]);
intperms(Digits, Xs,Nex, M, Len, Perms) ->
    %X = lists:nth(Nex+1, Digits),
    Xn = lists:sublist(Digits, Nex, Len),
    Rest = lists:subtract(Digits, Xn),
    intperms(Digits, [Xn|Xs], M, Nex+1, Len, [Rest|Perms]).

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
    
