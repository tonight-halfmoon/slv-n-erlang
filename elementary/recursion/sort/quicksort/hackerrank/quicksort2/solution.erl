-module(solution).
-export([main/0, qs/1]).
-include_lib("eunit/include/eunit.hrl").

main () ->
    print(qs([5,8,1,3,7,9,2])),
    true.

qs([])-> [];
qs([Pivot|T]) ->
    {Left, Right} = split([Pivot|T]),
    lists:append([qs(Left), [Pivot], qs(Right)])
    .

split([] ) -> [];
split([H|T]) ->
    split(T , H, [], []).
split([], _, Left, Right)->
    {Left, Right};
split([H|T], Pivot, Left, Right) ->
    case H < Pivot of 
	true ->
	     split(T,  Pivot, Left++[H], Right);
	false ->
	    split(T, Pivot, Left, [H|Right])
    end.

print(ok)-> ok;
print([])->
    [];
print([X])->
    io:format("~w~n", [X]),
    print([]);
print([H|T]) ->
    io:format("~w, ", [H]),
    print(T).
