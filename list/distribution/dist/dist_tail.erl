-module(dist_tail).
-include_lib("eunit/include/eunit.hrl").
-export([dist_tail/2]).

dist_one_with_empty_test_() ->
    {"Distribue [a] to an empty list must yield to [a]", ?_assertEqual([a], dist_tail([a], []))}.

dist_test_() ->
    {"Must distribute [1] to [2,3] and [3,2] and that yields in '[1,3,2],[1,2,3]'", ?_assertEqual([[1,2,3],[1,3,2]], dist_tail([1],[[2,3],[3,2]]))}.

dist_deep_test_() ->
    {"Must distribute [1] to [[[2,3], [9,8]], [3,2]] and that yields in '[1,3,2], [1,9,8], [1,2,3]'", ?_assertEqual([[1,3,2],[1,9,8],[1,2,3]], dist_tail([[1]],[[2,3],[9,8],[3,2]]))}.

dist_2deeplists_test_() ->
    {"Must distribute '[[1,9], [9,1]]' to '[[2,3], [5,6]]' and that yields in '[1,9,2,3], [1,9,5,6], [9,1,2,3], [9,1,5,6]'",
     ?_assertEqual( [[1,9,2,3],[1,9,5,6],[9,1,2,3],[9,1,5,6]], 
		   dist_tail([[1,9],[9,1]],[[2,3],[5,6]]))}.

dist_ListOfA_and_deeplists_test_() ->
{"Must distribute [7] to a List of Lists and first match",
 ?_assertMatch([X|_] when X == [7,3,1,2,6,5,4], dist_tail([[7]],[[2,3,1,4,6,5],[2,3,1,5,4,6],[2,3,1,6,5,4],[1,2,3,4,6,5],[1,2,3,5,4,6],[1,2,3,6,5,4],[3,1,2,4,6,5],[3,1,2,5,4,6],[3,1,2,6,5,4]]))}.

%[7,2,3,1,4,6,5]
%  [7,3,1,2,6,5,4]
%[7,1,2,3,6,5,4],

%dist_2500_must_halt_test_() ->
%    LoLs2500 = [lists:seq(1, 2500) || _ <- lists:seq(1, 2500)], 
%    {"distrbue a list of 2500 lists to a list of 2 lists  must halt; Each sublist is a 2500-element% list", 
 %    ?_assertMatch([[L|_]|_] when length(L) == 5000, dist_tail(LoLs2500, [lists:seq(1,2500),lists:seq(1,2500)]))}.


%dist_2500x2500_must_halt_test_() ->
%    LoLs2500 = [lists:seq(1, 2500) || _ <- lists:seq(1, 2500)], 
%    {"distrbue a list of 2500 lists to a list of 2500 lists  must halt; Each sublist is a 2500-element list", 
%     ?_assertMatch([L|_] when length(L) == 5000, dist_tail(LoLs2500, LoLs2500))}.


dist_tail([X], []) ->
    [X];
dist_tail([], [Y]) ->
    [Y];
dist_tail([X], [Y]) ->
    concat([X], [Y]);
dist_tail(L1, L2) ->
    dist_tail(L1, L2, []).

dist_tail([], [], D) ->
    D;
dist_tail(L1, L2, D) when length(L1) =< length(L2) - 2 ->
    dist_tail([], [], concat(lists:map(fun(Z) -> lists:flatten(Z) end, [[L1|Y] || Y <- L2]), D));
dist_tail(L1, L2, D) ->
    {LL1, LR1} = split(L1),
    {LL2, LR2} = split(L2),
    dist_tail([], [], concat(lists:map(fun(Z) -> lists:flatten(Z) end, [[X|Y] || X <- dist_tail(LR1, LL1), Y <- dist_tail(LR2, LL2)]), D)).

concat([], L) ->
    L;
concat(X, [H|_] = L) when not is_list(X) andalso not is_list(H) ->
    [X|L];
concat([H|T], L) ->
    concat(T, [H|L]).

split([]) ->
    {[], []};
split([X]) ->
    {[X], []};
split([H|T]) -> 
    split(T, [H]).

split(L, R) when length(L) =:= length(R);
		 length(L) =:= length(R) + 1 ->
    {R, L};
split([H|T], R) ->
    split(T, [H|R]).
