-module(udist).
-include_lib("eunit/include/eunit.hrl").
-export([udist/2]).

nor_test() ->
    ?assertEqual([[1,5,8,7,11],[9,0,8,7,11],[10,11,8,7]], udist([8,7,11],[[1,5],[9,0], [10,11]])).

empty_lsts_test() ->
    ?assertEqual([[],[]], udist([], [[],[]])).

empty_nor_test() ->
    ?assertEqual([[1,5],[9,0],[10,11]], udist([],[[1,5],[9,0], [10,11]])).

%%% Types:
%%% List = [X]
%%% ListOfLists = [List]
%%% Copy each element from first input List at the tail of every list in the second input ListOfLists while retain the original order.
%%% If the target list already has the same element, then the function won't duplicate
%%% Duplicates already exist in List won't be removed.

%%% Returns the same number of lists consumed from input ListOfLists parameter. Each list of ListOfLists will have extra elements according to the said functionality.

%%% How to achieve the same result using
%%% (1) List Comprehension and Erlang STD
%%% 10> [[X|Y] || X <- [[1,2,3]], Y <- [[5,7]]].
%%% [[[1,2,3],5,7]]
%%% 11> lists:flatten([[[1,2,3], 5,7]]).
%%% [1,2,3,5,7]
%%% In order to remove duplicates
%%% 16> sets:to_list(sets:from_list([1,2,3,5,7,1])).
%%% [3,2,5,1,7]
%%% Concrete 
%%% lists:map(fun(Z) -> lists:flatten(Z) end, [[X|Y] || X <- [[1,2,3]], Y <- [[9,8],[a,b],[x,y]]]).
%%% [[1,2,3,9,8],[1,2,3,a,b],[1,2,3,x,y]]
%%% 18> sets:to_list(sets:from_list(lists:flatten([[X|Y] || X <- [[1,2,3]], Y <- [[1,5,7]]]))).
%%%[3,2,5,1,7]
%%% 19> In order to remove duplicates in each sub-list
%%% lists:map(fun(U) -> sets:to_list(sets:from_list(U)) end, lists:map(fun(Z) -> lists:flatten(Z) end, [[X|Y] || X <- [[1,2,3,1]], Y <- [[9,8],[a,b,1],[x,y]]])).
%%% [[3,9,2,8,1],[3,a,b,2,1],[3,2,x,y,1]]
%%% Also, it works for input of ListOfLists in two parameters
%%% lists:map(fun(U) -> sets:to_list(sets:from_list(U)) end, lists:map(fun(Z) -> lists:flatten(Z) end, [[X|Y] || X <- [[1,2,3,1],[o]], Y <- [[9,8],[a,b],[x,y]]])).
%%% [[3,9,2,8,1],
%%%  [3,a,b,2,1],
%%%  [3,2,x,y,1],
%%%  [9,8,o],
%%%  [a,b,o],
%%%  [x,y,o]]

%%% (2) Using all Erlang STD 
%%% 2.1 - Map with fold
%%% 46> lists:flatten(lists:foldl(fun(X, Acc) -> lists:map(fun(Y) -> [Y,X] end,[[5,7]]) end, [], [[1,2,3]])).
%%% [5,7,1,2,3]
%%% 2.3. Map with map 
%%% 52> lists:flatten(lists:map(fun(Y) -> lists:map(fun(X) -> [Y,X] end,[[1,2,3]]) end, [[5,7]])).
%%% [5,7,1,2,3]
%%% 53> 
%%% 50> lists:map( fun(Z) -> lists:flatten(Z)end, lists:map(fun(X) -> lists:map(fun(Y) -> [X,Y] end, [[1,2,3]]) end, [[9,8],[a,b],[x,y]])).
%%% [[9,8,1,2,3],[a,b,1,2,3],[x,y,1,2,3]]
%%% 52> lists:map(fun(X) -> lists:map(fun(Y) -> lists:flatten( [X,Y]) end, [[1,2,3], [o]]) end, [[9,8],[a,b],[x,y]]).
%%% [[[9,8,1,2,3],[9,8,o]],
%%%  [[a,b,1,2,3],[a,b,o]],
%%%  [[x,y,1,2,3],[x,y,o]]]

udist(List, ListOfLists) -> 
    udist(List, ListOfLists, []).

udist(_Xs, [], U) -> 
    lists:reverse(U); 
udist(Xs, [[H|T]|OtherLists], U) when is_list(H) -> 
    udist(Xs, OtherLists, [udist(Xs, [H|T])|U]);
udist(Xs, [P|OtherLists], U) ->  
    udist(Xs, OtherLists, [umerge:umerge(P, Xs)|U]).

