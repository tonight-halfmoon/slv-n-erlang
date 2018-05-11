-module(foreach).
-export([for/1, even/1]).

for(N)->
    for(N, 0).
for(L, L) -> c;
for(N, _) ->
    for(N-1, 0).

even(N) ->
    even(N,[]).
even(0, Even_list) ->
    Even_list;
even(N, Even_list)->
    TF = even_test(N),
    case TF of
	true ->
	    even(N-1, [N|Even_list])
		;
	false ->
	    even(N-1, Even_list)
    end.

even_test(N) ->
    case N rem 2 =:= 0 of 
	true->
	    true
		;
       false ->
	    false
    end.
%;
%even_test(N) when N rem 2 =:= 0 -> true;
%even_test(N) when N rem 2 =/= 0->
%    false.

% if N rem 2 =:= 0 ->
%        true
%;
%      N rem 2 =/= 0 -> 
%           false
%    end.
