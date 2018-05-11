-module(solution).
-export([main/0]).
-include_lib("eunit/include/eunit.hrl").

qsplit3_test()->
    ?assertEqual([3,2,4,5,7], quicksortsplit([4,5,3,7,2])).
qsplit3_2_test()->
    ?assertEqual([-999,-4,900,5,3,7,2,1000], quicksortsplit([-4,-999,900,5,3,7,2, 1000])).
qsplit3_Test_Case_2_test()->
    ?assertEqual([2,10,3,7,9,4,6,12,8], quicksortsplit([2,10,3,7,9,4,6,12,8])).
main()->
    {ok, [N]} = io:fread("", "~d"),
    case verify_constraints_for_n(N) of 
	true ->
	    Ar = readxs(N),
	    case empty(Ar) of
		true ->
		    io:format("Wrong value entered for x;  must be -1000 =< x <=1000~n")
			;
		false ->
		   print(quicksortsplit(Ar))	    
	    end
		;
	false ->
	    io:format("Wrong n value; n must be 1 =< n <=1000~n")
    end.

quicksortsplit([]) ->
    [];
quicksortsplit([H|T]) ->
    qsplit3([H|T], H, [],[],[]).
qsplit3([H|T], Pivot, LS, EQ, GR) when H < Pivot ->
    qsplit3(T, Pivot, [H|LS], EQ, GR);
qsplit3([H|T], Pivot, LS, EQ, GR) when H =:= Pivot ->
    qsplit3(T, Pivot, LS, [H|EQ], GR);
qsplit3([H|T], Pivot, LS, EQ, GR) when H > Pivot ->
    qsplit3(T, Pivot, LS, EQ, [H|GR]);
qsplit3([], _, LS, EQ, GR) ->
    reverse(LS) ++ EQ  ++ reverse(GR).

readxs(N) ->
    readxs(N, 0, []).
readxs(N, N, L) ->
    reverse(L);
readxs(N, Next, L) ->
    {ok, [X]} = io:fread("", "~d"),
    case verify_constraints_for_x(X) of
	true ->
	    readxs(N, Next+1, [X|L])
		;
	false ->
	    []
    end.

reverse([]) -> [];
reverse([H|T]) -> reverse(T, [H]).
reverse([],L) -> L;
reverse([H|T], L) -> reverse(T, [H|L]).

verify_constraints_for_n(N) ->
    case is_integer(N) andalso grte(N,1) andalso lse(N,1000) of
	true ->
	    true;
	false ->
	    false
    end.

grte(X,Y) ->
    X >= Y.

lse(X,Y)->
    X =< Y.

verify_constraints_for_x(X) ->
    case is_integer(X) andalso grte(X,-1000) andalso lse(X,1000) of
	true ->
	    true;
	false ->
	    false
	end.

empty(L)->
    L =:= [].

print([]) -> ok;
print([H|T])->
    io:format("~w ", [H]),
    print(T).
