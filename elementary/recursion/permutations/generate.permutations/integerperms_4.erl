%% Final revision on the 21st April 16; Jeddah; SA

-module(integerperms_4).
-export([main/0, main2/0, fact/1, union_while/2, list_2_integer/1, intperms/1, digits/1]).
-include_lib("eunit/include/eunit.hrl").

%%%% Problem Statement 

% # [1] Permutations divisible by 8
% You are given an integer N. Is there a permutation of that integer's digits that yields an integer divisible by 8? For example, if the number N= 123, then
% {123, 132, 213, 231, 312, 321} are possible permutations and 312 is divisible by 8.
% 
% Constraints
% 1 <= T <= 45
% 0 <= N <= 10^110
% 
% Input Format 
% The first line contains an integer T that gives the number of test cases.
% T lines follow, each containing one integer N.
% 
% Output Format
% For each test case, print YES if there exists at least one way of re-arranging its digits such that it is divisible by 8, and print NO otherwise.
% 
% Sample Input #OO
% 2
% 61
% 75
% 
% Sample Output #OO
% YES
% NO
%%%%%%%%%%%%%%

%%
%  Profiling
%  {Microseconds, Perms} = timer:tc(integerperms_4, intperms, [19876]), 
%    io:fwrite("Execution Time: ~w microseconds~n", [Microseconds]),
%    io:fwrite("Execution Time: ~w seconds~n", [(Microseconds*1000000)]),
%%

%integer_perms_1elm_test()->
%    ?assertEqual([[1]], intperms(1)).

integer_perms_1elm_test()->
    ?assertEqual([], intperms(1)).

intger_perms_2elms_test() ->
    ?assertEqual([[1,2],[2,1]], intperms(12)).

intger_perms_r2elms_test() ->
    ?assertEqual([[2,1],[1,2]], intperms(21)).

integer_perms_123_test() ->
    ?assertEqual([[1,2,3],
                  [1,3,2],
                  [2,1,3],
		  [2,3,1],
		  [3,1,2],
		  [3,2,1]], reverse(intperms(123))).

integer_perms_960_test() ->
    ?assertEqual([[0,6,9],
		  [0,9,6],
		  [6,0,9],
		  [6,9,0],
		  [9,0,6],
		  [9,6,0]], intperms(960)).

integer_perms_8516_test() ->
    Perms = intperms(8516),
    NoDupsPerms = rm_dups(Perms),
    ?assertEqual(Perms, NoDupsPerms).

integer_perms_19876_test() ->
    Perms = intperms(19876),
    NoDupsPerms = rm_dups(Perms),
    ?assertEqual(Perms, NoDupsPerms).

integer_perms_719036_test() ->
    Perms = intperms(719036),
    NoDupsPerms = rm_dups(Perms),
    ?assertEqual(Perms, NoDupsPerms).

integer_perms_9870651_test() ->
    Perms = intperms(9870651),
    NoDupsPerms = rm_dups(Perms),
    ?assertEqual(Perms, NoDupsPerms).

main() ->
    {ok, [Int]} = io:fread("", "~d"),
    println(intperms(digits(Int))),
    true.

main2() ->
    {ok, [Int]} = io:fread("", "~d"),
    print_divisible_by_8(intperms(digits(Int))),
    true.

intperms(Integer) when is_integer(Integer) -> Digits = digits(Integer), intperms_divide(Digits, [], length(Digits), 0, []);
intperms(Digits) ->  intperms_divide(Digits, [], length(Digits), 0, []).

intperms_divide([X,Y], _, 2, _, _) -> [[X,Y],[Y,X]];
intperms_divide(_, Xs, M, M, Tents) -> intperms_conquer(Xs, Tents);
intperms_divide(Digits, Xs, M, Nex, Tents) ->
    X = lists:nth(Nex+1, Digits),
    Rest = lists:subtract(Digits, [X]),
    intperms_divide(Digits, [X|Xs], M, Nex+1, [Rest|Tents]).

intperms_conquer(Xs, Tents) -> intperms_conquer(Xs, Tents, []).

intperms_conquer(Xs, [], Perms) ->  union(Xs,Perms);
%intperms_conquer([X|Xs], [[H,T]|Tents], Perms) -> intperms_conquer(Xs, Tents, [[X,H,T],[X,T,H]|Perms]);
intperms_conquer(Xs, [T|Tents], Perms) -> intperms_conquer(Xs, Tents, intperms(T)++Perms).% [intperms(T)|Perms]).

union(Xs, Perms) -> union(Xs, Perms, []).

union(_Xs, [], U) -> U; 
union(Xs, [[H|T]|Perms], U) when is_list(H) -> union(Xs, Perms, [union(Xs, [H|T])|U]);
union(Xs, [P|Perms], U) ->  union(Xs, Perms, [union_while(P,Xs)|U]).

union_while(Perms,Xs)-> union_while(Perms,Xs, Perms).

union_while(_Perms,[], U) -> U;
union_while(Perms, [X|Xs], U) ->   case nothave(X, Perms) of 
				       true ->
					   union_while(Perms, Xs, [X|U]);
				       false ->
					   union_while(Perms, Xs, U)
				   end.

nothave(_X, []) -> true;
nothave(X, [X|_s]) -> false;
nothave(X, [_Y|Xs]) -> nothave(X, Xs).

digits(N) when N>=0 andalso N=<9 -> [N];
digits(N) -> digits(N, []).

digits(0, Ds) -> Ds;
digits(N, Ds) -> digits(trunc(N/10), [trunc(N rem 10)|Ds]).

println([]) -> ok;
println(Perms) -> println(Perms, 0).

println([], C) -> io:fwrite("Total permutations: ~w~n", [C]);
println([H|Perms], C)  -> io:fwrite("~w~n", [list_2_integer(H)]),
			  println(Perms, C+1).

print_divisible_by_8([]) -> io:fwrite("NO~n"); 
print_divisible_by_8([H|Perms])  -> Perm = list_2_integer(H),
				    case Perm rem 8 =:= 0 of
					true ->
					    io:fwrite("YES~n");
					false ->
					    print_divisible_by_8(Perms) 
				    end.

rm_dups(L) -> rm_dups(L, []).

rm_dups([H|T], Nodups) ->
    case exists(H, Nodups) of
	true ->
	    rm_dups(T, Nodups);
	false ->
	    rm_dups(T, lists:append([Nodups, [H]]))
    end;
rm_dups([], Nodups) -> Nodups.

exists(X,[X|_])-> true;
exists(X,[_|T]) -> exists(X,T);
exists(_,[]) -> false.

list_2_integer([]) -> 0;
list_2_integer([X|Xs]) -> list_2_integer(Xs, X, 1).

list_2_integer([], X, _) -> X; 
list_2_integer([X|Xs], X0, I) -> list_2_integer(Xs, X0 + (X*pow(10,I)), I+1).
    
pow(_N, K) when K == 0-> 1;
pow(N, K) ->  pow(N, K, 1, N).

pow(_N, K,K, R )-> R;
pow(N, K, I, R) -> pow(N, K, I+1, R*N).


reverse(S) -> reverse(S, []).
reverse([], R) -> R;
reverse([X|Xs], R) -> reverse(Xs, [X|R]).


fact(0)-> 1;
fact(N) when N > 0 -> N* fact(N-1).
