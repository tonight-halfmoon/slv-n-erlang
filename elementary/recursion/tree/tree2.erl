-module(tree2).
-export([insert/3, lookup/2, empty/0]).
-include_lib("eunit/include/eunit.hrl").

insert_test() ->
    ?assertEqual({ok, amk}, lookup(5,{node, {5,amk,{node, 'nil'}, {node, 'nil'}}})).

empty()->
    {node, 'nil'}.

insert(K, V, {node,'nil'}) ->
    {node, {K, V, {node, 'nil'},{node, 'nil'}}};
insert(New_key, New_value, {node,{Key, Value, Left, Right}})
  when New_key < Key ->
    {node, {Key, Value, insert(New_key,New_value, Left), Right}};
insert(New_k, New_v, {node, {K, V, Left, Right}}) when New_k > K ->
    {node, {K, V, Left, insert(New_k, New_v, Right)}};
insert(Key, Val, {node, {Key, _, Left, Right}}) ->
    {node, {Key, Val, Left, Right}}.

lookup(_, {node, 'nil'}) ->
    not_found;
lookup(K, {node, {K, V, _, _}}) ->
    {ok, V};
lookup(K, {node, {Nk, _, Left, Right}}) ->
    case ls(K, Nk) of 
	true ->
	    lookup(K, Left);
	false->
	    lookup(K, Right)
    end.
ls(X, Y)->
    X<Y.

    


