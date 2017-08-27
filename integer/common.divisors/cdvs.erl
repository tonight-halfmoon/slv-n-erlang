-module(cdvs).
-include_lib("eunit/include/eunit.hrl").
-export([main/0, cmndvsrs/2]).

cdvs_tc001_test() ->
    ?assertEqual({2,[1,2]}, cmndvsrs(10,4)).

cdvs_tc003_test() ->
    ?assertEqual({10,[1,2,3,4,6,8,12,16,24,48]}, cmndvsrs(288,240)).

cdvs_tc002_test() ->
    ?assertEqual({1,[1]}, cmndvsrs(1,100)).

cdvs_1_1_test() ->
    ?assertEqual({1,[1]}, cmndvsrs(1,1)).

cdvs_0_1_test() ->
    ?assertEqual({2,[1,11]}, cmndvsrs(0,11)).

cdvs_of_minus_and_minus_is_test() ->
    ?assertEqual({4,[1,2,4,8]}, cmndvsrs(-8,-24)).

cdvs_of_zeros_is_test() ->
    ?assertEqual({unlimited, infinity}, cmndvsrs(0,0)).

cmndvsrs(X,Y) ->
    GCD = gcd(X,Y),
    case GCD of 
	infinity ->
		{unlimited, infinity}; 
	_ ->
	    cmndvsrs(X, Y, 3, GCD, trunc(GCD/2), [GCD], 1)
    end.

cmndvsrs(_, _, _, 1, _, Cdvs, Acc) ->
    {Acc, Cdvs};
cmndvsrs(_, _, _, _, 1, Cdvs, Acc)-> 
    {Acc+1, [1|Cdvs]};
cmndvsrs(X, Y, Nex_step, GCD, Divisor, [Divisor|TCdvs], Acc) ->
    cmndvsrs(X, Y, Nex_step+1, GCD, trunc(GCD/Nex_step), [Divisor|TCdvs], Acc) ;
cmndvsrs(X, Y, Nex_step, GCD, Divisor, Cdvs, Acc) when X rem Divisor =:= 0 andalso Y rem Divisor =:= 0 ->
    cmndvsrs(X, Y, Nex_step+1, GCD, trunc(GCD/Nex_step), [Divisor|Cdvs], Acc+1);
cmndvsrs(X, Y, Nex_step, GCD, _, Cdvs, Acc) ->
    cmndvsrs(X, Y, Nex_step+1, GCD, trunc(GCD/Nex_step), Cdvs, Acc).

main() ->
    {ok,[T]} = io:fread("", "~d"),
    process(read_list(T)),
    true.

process(L) -> 
    process(L, []).

process([], Cdvs) ->  
    Cdvs;
process([H|T], Cdvs) ->
    [M|LT] = H,
    [L|_] = LT,
    Cdvsith = cmndvsrs(M, L),
    io:fwrite("~w~n", [Cdvsith]),
    process(T, Cdvs ++ [Cdvsith]).

gcd(0, 0) ->
    infinity;
gcd(X, 0) ->
    abs(X);
gcd(X,X) ->
    abs(X);
gcd(X,Y) ->
    gcd(Y, mod(X,Y)).

mod(0, 0) ->
    undefined;
mod(_, 0) ->
    undefined;
mod(0, _) ->
    0;
mod(_, 1) ->
    0;
mod(1, _) ->
    1;
mod(X,X) ->
    0;
mod(X, Y) ->
    ((X rem Y) + Y) rem Y.

read_list(T) -> 
    read_list(T, 0, []).

read_list(0, _, L) -> 
    L;
read_list(T, T, L) -> 
    L;
read_list(T, I, L) ->
    case io:get_line("") of
	eof ->
	    ok;
	Line ->
	    read_list(T, I+1, L ++ [lists:map(fun(X) -> list_to_integer(X) end, string:tokens(Line, "\n\r\t "))])
    end.

