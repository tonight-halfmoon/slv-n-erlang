-module(trycatch).
-export([do/1]).

do(Args) ->
    try Args/1 of
	V when is_integer(V) ->
	    V;
	R ->
	    io:format("We got R: ~p.~n", [R])
    catch
	E:_Detail ->
	    io:format("We caught E: ~p.~n", [E])
    end.
