-module(count).
-include_lib("eunit/include/eunit.hrl").
-export([cnt/1]).

count_empty_lst_test() ->
    ?assertEqual(0, cnt([])).

count_one_elm_lst_test() ->
    ?assertEqual(1, cnt([1])).

count_8_elm_lst_test() ->
    ?assertEqual(8, cnt([1,2,3,4,5,6,7,8])).

count_list_of_elm_lst_test() ->
    ?assertEqual(8, cnt([[1],[2,3,4],[5],6,7,8])).

count_list_of_elm_lst_deeper_test() ->
    ?assertEqual(15, cnt([[1],[2,3,4],[5],6,[9,[7,0,[2,[4,[4,[44,5]]]]]],8])).

cnt(List) ->
    cnt(List, 0).

cnt([], Acc) ->
    Acc;
cnt([H|T], Acc) ->
    case is_list(H) of 
	true ->
	    cnt(T, Acc + cnt(H));
	false -> 
	    cnt(T, Acc + 1)
    end.
