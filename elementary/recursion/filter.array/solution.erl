-module(solution).
-export([main/0]).
-include_lib("eunit/include/eunit.hrl").

filter_test()->
    ?assertEqual([2,1,0], filter_(3, [10,9,8,2,7,5,1,3,0])).

main() ->
    io:fwrite("~w~n",[ filter_(3, [4,6,1,0])]),
    true.

filter_(Dl, L) ->
    filter_(fun dlmtr/2, Dl, L).

filter_(_, _, []) -> 
    [];
filter_(Pred, Dl, [H|T]) -> 
    %[Pred(H,Dl)| filter_(Pred, Dl,T)].
    case Pred(H,Dl) of 
	true ->
	    [H| filter_(Pred, Dl,T)];
	false ->
	    filter_(Pred, Dl, T)
    end.
dlmtr(X,Dl) when X < Dl ->
    true;
dlmtr(X,Dl) when X >= Dl ->
    false.
    
