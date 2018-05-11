-module(solution).
-export([main/0, pascal_triangle/1]).

main() ->
    {ok, [K]} = io:fread("", "~d"),
    pascal_triangle(K),
    true.

pascal_triangle(K) ->
    pascal_triangle(K, 0, 0, []).
pascal_triangle(K, K, _, DDL)->
    DDL;
pascal_triangle(KL, _, _, _) when KL < 2 orelse KL > 10 ->
    [];
pascal_triangle(K, N, R, DDL) ->
    Nth_row = calc_binomial_coeff(N,R, []),
    print_output(Nth_row),
    pascal_triangle(K, N+1, R, [DDL|Nth_row]).

calc_binomial_coeff(N,N,L) ->
    lists:append([L, [eval(N,N)]]);
calc_binomial_coeff(N, R, L) -> 
    calc_binomial_coeff(N, R+1, lists:append([L, [eval(N,R)]])).

eval(N,R)->
   trunc(fact(N)/ ( fact(R) * fact(N-R))).

fact(N) ->
    fact(N, 1).
fact(0, Fact)->
    Fact;
fact(1, Fact) ->
    Fact;
fact(N, Acc) ->
    fact(N-1, N * Acc).

print_output([]) ->
    io:fwrite("~n");
print_output(L) when is_list(L) ->
    [H|T] = L,
    print_output(H),
    print_output(T);
print_output(X) ->
    io:fwrite("~w ", [X]).
