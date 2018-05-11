-module(solution_acc).
-export([main/0, cds/2, gcd/2]).
-include_lib("eunit/include/eunit.hrl").

cdvs_tc001_test() ->
    ?assertEqual(2, cds(10,4)).
cdvs_tc003_test() ->
    ?assertEqual(10, cds(288,240)).
cdvs_tc002_test() ->
    ?assertEqual(1, cds(1,100)).
cdvs_1_1_test() ->
    ?assertEqual(1, cds(1,1)).

main() ->
    {ok,[T]} = io:fread("", "~d"),
    process(read_list(T)),
    true.

cds(X,Y) ->
    GCD = gcd(X,Y),
    cds(X, Y, 3, GCD, trunc(GCD/2), [GCD], 1).
cds(_,_,_,1,_,_, Acc) ->
    Acc;
cds(_, _, _, _, 1, _, Acc)-> 
   Acc+1;
cds(X,Y, Nex_step, GCD, Divisor, [Divisor|TCdvs], Acc) ->
    cds(X,Y, Nex_step+1, GCD, trunc(GCD/Nex_step),[Divisor|TCdvs], Acc) ;
cds(X, Y, Nex_step, GCD, Divisor, Cdvs, Acc) when X rem Divisor =:= 0 andalso Y rem Divisor =:= 0 ->
    cds(X, Y, Nex_step+1, GCD, trunc(GCD/Nex_step), [Divisor|Cdvs], Acc+1);
cds(X, Y, Nex_step, GCD, _, Cdvs, Acc) ->
    cds(X, Y, Nex_step+1, GCD, trunc(GCD/Nex_step), Cdvs, Acc).

gcd(X,0)->
    X;
gcd(X,X) ->
    X;
gcd(X,Y) ->
    gcd(Y,mod(X,Y)).

mod(X,X) ->
    0;
mod(_, 1) ->
    0;
mod(X, Y) ->
    X rem Y.

read_list(T) -> read_list(T, 0, []).
read_list(0, _, L) -> L;
read_list(T, T, L) -> L;
read_list(T, I, L) ->
    case io:get_line("") of
	eof ->
	    ok;
	Line ->
	    read_list(T, I+1, L ++ [lists:map(fun(X) -> list_to_integer(X) end, string:tokens(Line, "\n\r\t "))])
    end.

process(L) -> process(L, []).
process([], Cdvs) ->  Cdvs;
process([H|T], Cdvs) ->
    [M|LT] = H,
    [L|_] = LT,
    Cdvsith = cds(M, L),
    io:fwrite("~w~n", [Cdvsith]),
    process(T, Cdvs ++ [Cdvsith]).

