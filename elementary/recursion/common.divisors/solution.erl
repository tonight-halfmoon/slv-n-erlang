-module(solution).
-export([main/0, cds/2]).
-include_lib("eunit/include/eunit.hrl").

cdvs_tc001_test() ->
    ?assertEqual([1,2], cds(10,4)).
cdvs_tc003_test() ->
    ?assertEqual([1,2,3,4,6,8,12,16,24,48], cds(288,240)).
cdvs_tc002_test() ->
    ?assertEqual([1], cds(1,100)).
cdvs_1_1_test() ->
    ?assertEqual([1], cds(1,1)).

main() ->
    {ok,[T]} = io:fread("", "~d"),
    process(read_list(T)),
    true.

cds(X,Y) ->
    GCD = gcd(X,Y),
    cds(X, Y, 3, GCD, trunc(GCD/2), [GCD]).

cds(_,_,_,1,_,Cdvs) ->
    Cdvs;
cds(_, _, _, _, 1, Cdvs)-> 
    [1|Cdvs];
cds(X,Y, Nex_step, GCD, Divisor, [Divisor|TCdvs]) ->
    cds(X,Y, Nex_step+1, GCD, trunc(GCD/Nex_step),[Divisor|TCdvs]) ;
cds(X, Y, Nex_step, GCD, Divisor, Cdvs) when X rem Divisor =:= 0 andalso Y rem Divisor =:= 0 ->
    
    cds(X, Y, Nex_step+1, GCD, trunc(GCD/Nex_step), [Divisor|Cdvs]);
cds(X, Y, Nex_step, GCD, _, Cdvs) ->
    cds(X, Y, Nex_step+1, GCD, trunc(GCD/Nex_step), Cdvs).

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
    %io:fwrite("M:~w , L:~w~n", [M,L]),
    Cdvsith = cds(M, L),
    io:fwrite("~w~n", [Cdvsith]),
    process(T, Cdvs ++ [Cdvsith]).

