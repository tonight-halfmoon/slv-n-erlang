-module(integerperms_3).
-export([main/0, fact/1, union_while/2]).
-include_lib("eunit/include/eunit.hrl").

intperms_test() ->
    ?assertEqual([[1,2,3],
                  [1,3,2],
                  [2,1,3],
		  [2,3,1],
		  [3,1,2],
		  [3,2,1]], intperms(digits(123))).

inteperms_2_test() ->
    Perms = intperms(digits(19876)),
    NoDupsPerms = rm_dups(Perms),
    ?assertEqual(Perms, NoDupsPerms).

main() ->
    {ok, [Int]} = io:fread("", "~d"),
    println(intperms(digits(Int))),
    true.

intperms(Digits) ->  intperms(Digits, [], length(Digits), 0, []).

intperms([], _, _, _, _) -> true;
intperms([X,Y], _, 2, _, _) -> [[X,Y],[Y,X]];
intperms(_, Xs, M, M, Tents) -> intperms_final(Xs, Tents);
intperms(Digits, Xs, M, Nex, Tents) ->
    X = lists:nth(Nex+1, Digits),
    Rest = lists:subtract(Digits, [X]),
    intperms(Digits, [X|Xs], M, Nex+1, [Rest|Tents]).

intperms_final(Xs, Tents) -> intperms_final(Xs, Tents, []).

intperms_final([], [], Perms) -> Perms;
intperms_final(Xs, [], Perms) ->  union(Xs,Perms);
intperms_final([X|Xs], [[H]|Tents], Perms) ->  intperms_final(Xs, Tents, [[H,X], [X,H]|Perms]);
intperms_final([X|Xs], [[H,T]|Tents], Perms) -> intperms_final(Xs, Tents, [[X,H,T],[X,T,H]|Perms]);
intperms_final(Xs, [T|Tents], Perms) -> intperms_final(Xs, Tents, [intperms(T)|Perms]).

digits(N) when N>=0 andalso N=<9 -> [N];
digits(N) -> digits(N, []).

digits(0, Ds) -> Ds;
digits(N, Ds) -> digits(trunc(N/10), [trunc(N rem 10)|Ds]).

union(Xs, Perms) -> union(Xs, Perms, []).

union(_Xs, [], U) -> U; 
union(Xs, [[H|T]|Perms], U) when is_list(H) ->
    UNION =  U ++ union(Xs, [H|T]),
    union(Xs, Perms, UNION);
union(Xs, [P|Perms], U) ->
    UNION = [union_while(P,Xs)|U],
    union(Xs, Perms, UNION ).

fact(0)-> 1;
fact(N) when N > 0 -> N* fact(N-1).

union_while(Perms,Xs)-> union_while(Perms,Xs,[]).
union_while(Perms,[], U) -> U++Perms;
union_while(Perms, [X|Xs], U) -> 
    case nothave(X, Perms) of 
	true ->
	    union_while(Perms, Xs, [X|U]);
	false ->
	    union_while(Perms, Xs, U)
    end.

nothave(_X, [])-> true;
nothave(X, [X|_s]) -> false;
nothave(X, [_Y|Xs]) -> nothave(X, Xs).
 
println([]) ->
    ok;
println(Perms) ->
    println(Perms, 0).

println([], C) ->
    io:fwrite("Total permutations: ~w~n", [C]);
println([H|Perms], C)  ->
    io:fwrite("~w~n", [H]),
    println(Perms, C+1).

    

rm_dups(L)->
    rm_dups(L, []).

rm_dups([H|T], Nodups) ->
    case exists(H, Nodups) of
	true ->
	    rm_dups(T, Nodups);
	false ->
	    rm_dups(T, lists:append([Nodups, [H]]))
    end;
rm_dups([], Nodups) ->
    Nodups.

exists(X,[X|_])->
    true;
exists(X,[_|T]) ->
    exists(X,T);
exists(_,[]) ->
    false.
