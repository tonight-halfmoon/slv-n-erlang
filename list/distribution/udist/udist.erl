-module(udist).
-include_lib("eunit/include/eunit.hrl").
-export([udist/2]).

%%% Types:
%%% List = [X]
%%% ListOfLists = [List]
%%% Copy each element from first input List at the tail of every list in the second input ListOfLists while retain the original order.
%%% If the target list already has the same element, then the function won't duplicate
%%% Duplicates already exist in List won't be removed.

%%% Returns the same number of lists consumed from input ListOfLists parameter. Each list of ListOfLists will have extra elements according to the said functionality.


nor_test() ->
    ?assertEqual([[1,5,8,7,11],[9,0,8,7,11],[10,11,8,7]], udist([8,7,11],[[1,5],[9,0], [10,11]])).

empty_lsts_test() ->
    ?assertEqual([[],[]], udist([], [[],[]])).

empty_nor_test() ->
    ?assertEqual([[1,5],[9,0],[10,11]], udist([],[[1,5],[9,0], [10,11]])).

udist(List, ListOfLists) -> 
    udist(List, ListOfLists, []).

udist(_Xs, [], U) -> 
    lists:reverse(U); 
udist(Xs, [[H|T]|OtherLists], U) when is_list(H) -> 
    udist(Xs, OtherLists, [udist(Xs, [H|T])|U]);
udist(Xs, [P|OtherLists], U) ->  
    udist(Xs, OtherLists, [umerge:umerge(P, Xs)|U]).

