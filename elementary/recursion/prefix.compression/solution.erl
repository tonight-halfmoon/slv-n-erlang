-module(solution).
-export([main/0]).
-include_lib("eunit/include/eunit.hrl").

prefix_cmprss_test() -> 
    ?assertEqual({[a,b,c], [d,e,f,p,r], [p,q,r]}, prefix_cmprss([a,b,c,d,e,f,p,r],[a,b,c,p,q,r])).
prefix_cmprss_kitkat_kit_test() ->
    ?assertEqual({[k,i,t], [k,a,t], []}, prefix_cmprss([k,i,t,k,a,t],[k,i,t])).

main() ->
    {ok, [X]} = io:fread("", "~s"),
    {ok, [Y]} = io:fread("", "~s"),
    {Prefix, Xn, Yn} = prefix_cmprss(X, Y),
    %io:fwrite("~s ~s ~s ~n", [Prefix, Xn, Yn]),
    io:fwrite("~w ~s~n", [length(Prefix), Prefix]),
    io:fwrite("~w ~s~n", [length(Xn), Xn]),
    io:fwrite("~w ~s~n", [length(Yn), Yn]),    
    true.

prefix_cmprss(X, Y) ->
    prefix_cmprss(X, Y, []).
prefix_cmprss(X, [], PXnYn) ->
    {reverse(PXnYn), X, []};
prefix_cmprss([], Y, PXnYn) ->
    {reverse(PXnYn), [], Y};
prefix_cmprss([H|TX], [H|TY], PXnYn) ->
    prefix_cmprss(TX, TY, [H|PXnYn]);
prefix_cmprss([HX|TX], [HY|TY], PXnYn) ->
    {reverse(PXnYn), [HX|TX], [HY|TY]}.

reverse(L) ->
    reverse(L, []).
reverse([H|T], R) ->
    reverse(T, [H|R]);
reverse([], R) -> R.
