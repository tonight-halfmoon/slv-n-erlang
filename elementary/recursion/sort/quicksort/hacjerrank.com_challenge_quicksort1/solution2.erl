-module(solution2).
-export([main/0]).
-include_lib("eunit/include/eunit.hrl").

qsplit3_test()->
    ?assertEqual([2,3,4,7,5], quicksortsplit([4,5,3,7,2])).

main()->
    {ok, _} = io:fread("", "~d"),
    Ar = readxs(),
    case empty(Ar) of
	true ->
	    io:format("Wrong value entered for x;  must be -1000 =< x <=1000~n")
		;
	false ->
	    print(quicksortsplit(Ar))	    
    end
	.

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

readxs() ->
    Xs_data = io:get_line(""),
    Xs = lists:map(fun erlang:list_to_integer/1, string:tokens(string:strip(Xs_data, right, $\n), " ")),
    case verify_constraints_for_xs(Xs) of
	true ->
	    Xs
		;
	false ->
	    []
    end.

reverse([H|T])->
    reverse(T, [H]).
reverse([],L)->
    L;
reverse([H|T], L) ->
    reverse(T, [H|L]).

%verify_constraints_for_n(N) ->
%    case is_integer(N) andalso grte(N,1) andalso lse(N,1000) of
%	true ->
%	    true;
%	false ->
%	    false
%    end.

grte(X,Y) ->
    X>=Y.

lse(X,Y)->
    X=<Y.

verify_constraints_for_xs([X|T]) ->
    verify_constraints_for_xs(T, verify_constraints_for_x(X)).
verify_constraints_for_xs([X|T], true) ->
    verify_constraints_for_xs(T, verify_constraints_for_x(X));
verify_constraints_for_xs(_, false) ->
    false;
verify_constraints_for_xs([], LastVerf) ->
    LastVerf.

verify_constraints_for_x(X)->
    case is_integer(X) andalso grte(X,-1000) andalso lse(X,1000) of
	true ->
	    true;
	false ->
	    false
	end.

empty(L)->
    L =:=[].

print([])->
    ok;
print([H|T])->
    io:format("~w ", [H]),
    print(T).
