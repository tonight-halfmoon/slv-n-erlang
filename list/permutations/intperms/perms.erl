%%%-------------------------------------------------------------------
%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2017, 
%%% @doc
%%% Original implementation on the 21st April 16; Jeddah; SA
%%% 4th Oct. 2017 -> Removed length(L) as considered expensive. Utilising two copies of the original list for the entry. Then, recurring with the Tail. X will be derived from the L--[X], while X is the head the T from the former round.

%%% Another problem to solve here is All Permutations divisible by 8. Check fun 'main2'. 
%%% @end
%%%------------------------------------------------------------------- 
-module(perms).
-export([main/0, main2/0, permute/1, rm_duplicates/1, reverse/1, exists/2]).
-include_lib("eunit/include/eunit.hrl").

permute_1_test() ->
    ?assertEqual([[1]], permute(1)).

permute_empty_test() ->
    ?assertEqual([], permute([])).

permute_2_test() ->
    ?assertEqual([[1,2],[2,1]], permute(12)).

permute_r2_test() ->
    ?assertEqual([[2,1],[1,2]], permute(21)).

%%% Compute Permutations of an integer having n distinct digits
permute(I) when is_integer(I) -> 
    permute(digits(I));
permute(L) ->  
    permute_divide(L, L, [], []).

permute_divide([X], _, _, _) ->
    [[X]];
permute_divide([X,Y], _,  _, _) -> 
    [[X,Y],[Y,X]];
permute_divide(_, [], Xs, Tents) -> 
    permute_conquer(Xs, Tents);
permute_divide(L, [X|T], Xs, Tents) ->
    permute_divide(L, T, [X|Xs], [L--[X]|Tents]).

permute_conquer(Xs, Tents) -> 
    permute_conquer(Xs, Tents, []).

permute_conquer(_, [], Perms) ->
    Perms;
permute_conquer(Xs, [T|Tents], Perms) ->
    permute_conquer(Xs, Tents, distrib(Xs, lists:append(permute(T), Perms))).

%%% Copy each element from the first input list into every list in the second input list of lists
%%% If the target list already has the same element, then the function won't duplicate
%%% Duplicate elements in the source list will be copied if the target list does not have the same element
%%% Xs: List
%%% LPerms: List of Lists  
distrib(Xs, LPerms) -> 
    distrib(Xs, LPerms, []).

distrib(_Xs, [], U) -> 
    U; 
distrib(Xs, [[H|T]|LPerms], U) when is_list(H) -> 
    distrib(Xs, LPerms, [distrib(Xs, [H|T])|U]);
distrib(Xs, [P|LPerms], U) ->  
    distrib(Xs, LPerms, [union(P, Xs)|U]).

%%% Union two lists. Transforms two lists into one list w.r.t the definition of Union. However, the problem is on lists and not on sets.
%%% Perm must be a list of elements. Element must not be a list. Therefore, A list of lists is not expected.
%%% Xs must be a list of elements. Element must not be a list. Therefore, a list of lists is not expected. 
union(Perm, Xs) -> 
    union(Perm, Xs, Perm).

union(_Perm, [], U) -> 
    U;
union(Perm, [X|Xs], U) ->   
    case exists(X, Perm) of 
	false ->
	    union(Perm, Xs, [X|U]);
	true ->
	    union(Perm, Xs, U)
    end.

exists(_, []) ->
    false;
exists(X, [X|_]) ->
    true;
exists(X, [_|T]) ->
    exists(X, T).

rm_duplicates(L) ->
    rm_duplicates(L, []).

rm_duplicates([H|T], Nodups) ->
    case exists(H, Nodups) of
	true ->
	    rm_duplicates(T, Nodups);
	false ->
	    rm_duplicates(T, lists:append([Nodups, [H]]))
    end;
rm_duplicates([], Nodups) ->
    Nodups.

reverse(S) ->
    reverse(S, []).
reverse([], R) ->
    R;
reverse([X|Xs], R) ->
    reverse(Xs, [X|R]).

digits(N) when 0 > N ->
    digits(abs(N));
digits(N) when N >= 0 andalso N =< 9 ->
    [N];
digits(N) ->
    digits(N, []).

digits(0, Ds) ->
    Ds;
digits(N, Ds) ->
    digits(trunc(N/10), [trunc(N rem 10)|Ds]).

main() ->
    {ok, [Int]} = io:fread("", "~d"),
    println(permute(digits(Int))),
    true.

println([]) -> 
    ok;
println(Perms) -> 
    println(Perms, 0).

println([], C) -> 
    io:fwrite("Total permutations: ~w~n", [C]);
println([H|Perms], C) -> 
    io:fwrite("~w~n", [list_2_integer(H)]),
    println(Perms, C+1).

list_2_integer([]) -> 
    0;
list_2_integer([X|Xs]) -> 
    list_2_integer(Xs, X, 1).

list_2_integer([], X, _) -> 
    X; 
list_2_integer([X|Xs], X0, I) -> 
    list_2_integer(Xs, X0 + (X * pow(10, I)), I+1).
    
pow(_N, K) when K == 0 -> 
    1;
pow(N, K) ->  
    pow(N, K, 1, N).

pow(_N, K, K, R) ->
    R;
pow(N, K, I, R) -> 
    pow(N, K, I+1, R*N).


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

main2() ->
    {ok, [Int]} = io:fread("", "~d"),
    print_divisible_by_8(permute(digits(Int))),
    true.

print_divisible_by_8([]) -> 
    io:fwrite("NO~n"); 
print_divisible_by_8([H|Perms]) -> 
    Perm = list_2_integer(H),
    case Perm rem 8 =:= 0 of
	true ->
	    io:fwrite("YES~n");
	false ->
	    print_divisible_by_8(Perms) 
    end.
