-module(dist_tail).
-include_lib("eunit/include/eunit.hrl").
-export([dist_tail/2]).

dist_one_with_empty_test_() ->
    {"Distribue [a] to an empty list must yield to [a]", ?_assertEqual([a], dist_tail([a], []))}.

dist_test_() ->
    {"Must distribute [1] to [2,3] and [3,2] and that yields in '[1,3,2],[1,2,3]'", ?_assertEqual([[1,3,2], [1,2,3]], dist_tail([1],[[2,3],[3,2]]))}.

dist_deep_test_() ->
    {"Must distribute [1] to [[[2,3], [9,8]], [3,2]] and that yields in '[1,3,2], [1,9,8], [1,2,3]'", ?_assertEqual([[1,3,2], [1,2,3], [1,9,8]], dist_tail([1],[[[2,3],[9,8]],[3,2]]))}.

dist_2deeplists_test_() ->
    {"Must distribute '[[1,9], [9,1]]' to '[[2,3], [5,6]]' and that yields in '[1,9,2,3], [1,9,5,6], [9,1,2,3], [9,1,5,6]'",
     ?_assertEqual( [[[9,1,2,3]],[[9,1,5,6]],[[1,9,5,6]],[[1,9,2,3]]], 
		   dist_tail([[1,9],[9,1]],[[2,3],[5,6]]))}.

dist_ListOfA_and_deeplists_test_() ->
{"Must distribute [7] to a List of Lists and first match",
 ?_assertMatch([X|_] when X == [7,1,2,3,6,5,4], dist_tail([7],[[2,3,1,4,6,5],[2,3,1,5,4,6],[2,3,1,6,5,4],[1,2,3,4,6,5],[1,2,3,5,4,6],[1,2,3,6,5,4],[3,1,2,4,6,5],[3,1,2,5,4,6],[3,1,2,6,5,4]]))}.

%[7,2,3,1,4,6,5]
%  [7,3,1,2,6,5,4]
%[7,1,2,3,6,5,4],

dist_2500_must_halt_test_() ->
    LoLs2500 = [lists:seq(1, 2500) || _ <- lists:seq(1, 2500)], 
    {"distrbue a list of 2500 lists to a list of 2 lists  must halt; Each sublist is a 2500-element list", 
     ?_assertMatch([[L|_]|_] when length(L) == 5000, dist_tail(LoLs2500, [lists:seq(1,2500),lists:seq(1,2500)]))}.


%dist_2500x2500_must_halt_test_() ->
%    LoLs2500 = [lists:seq(1, 2500) || _ <- lists:seq(1, 2500)], 
%    {"distrbue a list of 2500 lists to a list of 2500 lists  must halt; Each sublist is a 2500-element list", 
%     ?_assertMatch([L|_] when length(L) == 5000, dist_tail(LoLs2500, LoLs2500))}.


dist_tail([X], []) ->
    [X];
dist_tail([], [Y]) ->
    [Y];
dist_tail([X], [Y]) ->
    lists:map(fun(A) -> lists:map(fun(B) -> lists:flatten(A, B) end, [X]) end, [Y]);
dist_tail(L1, L2) ->
    dist_tail(L2, L1, []).

dist_tail([], [], D) ->
    D;
dist_tail(L1, L2, D) ->
    case length(L1) > length(L2) of 
	true ->
	    %{LL1, LR1} = lists:split(trunc(length(L1)/2), L1),
	    {LL1, LR1} = split(L1),
	    dist_tail([], [], concat(concat(dist_tail(LL1, L2), dist_tail(LR1, L2)), D));
	false ->
	    %{LL2, LR2} = lists:split(trunc(length(L2)/2), L2),
	    {LL2, LR2} = split(L2),
            dist_tail([], [], concat(concat(dist_tail(LL2, L1), dist_tail(LR2, L1)), D)) 
    end.

concat([], L) ->
    L;
concat(X, [H|T]) when not is_list(X) andalso not is_list(H) ->
    [X|[H|T]];
concat([H|T], L) ->
    concat(T, [H|L]).

split([]) ->
    [];
split([_]) ->
    [];
split([H|T]) -> 
    split(T, [H]).

split(L, R) when length(L) =:= length(R);
		 length(L) =:= length(R) + 1 ->
    {R, L};
split([H|T], R) ->
    split(T, [H|R]).
