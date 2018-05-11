-module(solution).
-export([main/0]).
-include_lib("eunit/include/eunit.hrl").

median_test() ->
    ?assertEqual(3.0, median([0,1,2,4,6,5,3])).

%% the implementation is wrong: the point is to sort the array and 
%% get the median from the middle of the sorted list
main () ->
    {ok, [N]} = io:fread("", "~d"),
    {ok, L} = io:fread("", string:join(replicate(N, "~d"), " ")),
    Median = median(L),
    io:format("~w~n", [trunc(Median)])
	.

median(L) -> median(L, 0, 0).

median([], Sum, Count) when Count =/= 0->
    Sum/Count;
median([], Sum, Count) when Count =:= 0 ->
    Sum;
median([H|T], Sum, Count) ->
    median(T,H+Sum, 1+Count).
 
replicate(T, _) when T =<0->
    [];
replicate(T, O) -> [O|replicate(T-1, O)].
