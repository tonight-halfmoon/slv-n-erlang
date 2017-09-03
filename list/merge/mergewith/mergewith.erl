-module(mergewith).
-include_lib("eunit/include/eunit.hrl").
-export([merge/3, mergeasc/2, mergedesc/2]).

merge_test() ->
    ?assertEqual([a,b,c,d], merge(fun lte/2, [a,b], [c,d])).
merge_@_test() ->
    ?assertEqual([a,b,c,d,e], merge(fun lte/2, [a,b], [c,d,e])).
mergeasc_test() ->
    ?assertEqual([1,2,3,4], mergeasc([1,2],[3,4])).
mergeasc__test() ->
    ?assertEqual([1,2,3,4], mergeasc([3,4],[1,2])).
mergedesc_test() ->
    ?assertEqual([4,3,2,1], mergedesc([4,3],[2,1])).
mergedesc__test() ->
    ?assertEqual([4,3,2,1], mergedesc([2,1],[4,3])).
merge_empty__test() ->
    ?assertEqual([], merge(fun () -> true end, [],[])).
merge_sol_with_empty__test() ->
    ?assertEqual([1], merge(fun (_, _) -> false end, [1], [])).
merge_empty_with_sol__test() ->
    ?assertEqual([1], merge(fun (_, _) -> false end, [], [1])).
merge_sol_with_2elmlst__test() ->
    ?assertEqual([4,2,1], merge(fun (_, _) -> false end, [1], [4,2])).
merge_same__test() ->
    ?assertEqual([1,1], merge(fun (_, _) -> false end, [1], [1])).
merge_or__test() ->
    ?assertEqual([5,12,1], merge(fun (_, _) -> true end, [5], [12,1])).
merge_originalorderpreserved__test() ->
    ?assertEqual([5,1,12], merge(fun (_, _) -> true end, [5], [1,12])).
merge_duplicatesallowed__test() ->
    ?assertEqual([5,12,1,12,12], merge(fun (_, _) -> true end, [5,12], [1,12,12])).

mergedesc(L1, L2) ->
    merge(fun gte/2, L1, L2).

mergeasc(L1, L2) ->
    merge(fun lte/2, L1, L2).

merge(Pred, [H1|T1], [H2|T2])->
    case Pred(H1, H2) of
	true ->
	    [H1|merge(Pred, T1, [H2|T2])];
        false ->
	    [H2|merge(Pred, [H1|T1], T2)]
    end;
merge(_, [], L2) ->
    L2;
merge(_, L1, []) ->
    L1.

lte (X, Y) -> X =< Y.
gte (X, Y) -> X >= Y.
