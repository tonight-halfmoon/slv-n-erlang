-module(countpairs_adjacents).
-export([cp_adj/1, cp_adj2/1]). 
-include_lib("eunit/include/eunit.hrl"). 
-include("include/testcase_input02_04.hrl").

cp_adj_test_() ->
    ?_assertEqual(99999, cp_adj(?testcase_input02_04())).

cp_adj2_test_() ->
    ?_assertEqual(99999, cp_adj2(?testcase_input02_04())).

is_pair([X,X]) ->
    1;
is_pair([_X,_Y]) ->
    0.

cp(L) ->
    cp(L, 0).

cp([], Acc) ->
    Acc;
cp([X,Y], Acc) ->  
    is_pair([X,Y]) + Acc;
cp([_H|[]], Acc) ->
    Acc;
cp([X,Y,Z|T], Acc) ->
    cp([Z|T], Acc + is_pair([X,Y]) + is_pair([Y,Z])).
    
cp_adj(L) when 3 >= length(L) ->
    cp(lists:flatten(L));
cp_adj(L) ->
    {L1, [HL2|TL2]} = lists:split(trunc(length(L)/2), L),
    cp_adj([L1|[HL2]]) + cp_adj([HL2|TL2]).


cp_adj2(L) ->
    cp_adj2(L, 0, 0).

cp_adj2([], _Acc1, Acc2) ->
    Acc2;
cp_adj2(L, _Acc1, Acc2) when 3 >= length(L) ->
    cp(lists:flatten(L)) + Acc2;
cp_adj2(L, Acc1, Acc2) ->
    {L1, [HL2|TL2]} = lists:split(trunc(length(L)/2), L),
    cp_adj2([L1|[HL2]], Acc1 + Acc2, Acc2 + cp_adj2([HL2|TL2])).
