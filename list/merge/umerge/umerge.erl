%%% Merge two lists while retain the original order and pick one copy of the element when two elements from each list compare equal.
%%% In case client module does not care about the original order then using sets library would be the optimal decision
%%% For example: 
%%% sets:to_list(sets:from_list(sets:to_list( sets:umerge( sets:from_list([1,5]) , sets:from_list([6,5,9,10,0]))))).
%%% Another method is to utilise list comprehension, for example:
%%% 84> lists:flatten([case lists:subtract(X,Y) of X -> [X,Y]; Diff -> lists:append(Diff,Y) end || X<-[[1,2,3,8]], Y <-[[4,5,9,1,2,8]]]).

-module(umerge).
-include_lib("eunit/include/eunit.hrl").
-export([umerge/2]).

umerge_empty_lists_test() ->
	?assertEqual([], umerge([],[])).

umerge_L1_with_empty_lst_test() ->
	?assertEqual([1,5], umerge([1,5],[])).

umerge_emptyL1_with_someL2_lst_test() ->
	?assertEqual([1,2], umerge([],[1,2])).

umerge_two_s_lsts_test() ->
	?assertEqual([1,5,6], umerge([1,5],[6,5])).

umerge_2_with_4elms_lsts_test() ->
	?assertEqual([1,5,6,9,10,0], umerge([1,5],[6,5,9,10,0])).


umerge_5_with_duplicatesinL1_with_lsts_test() ->
	?assertEqual([1,5,0,12,1,6,9,10], umerge([1,5,0,12,1],[6,5,9,10,0])).

%%% Umerge two lists. Returns one list w.r.t the definition of Union. However, the problem is on lists and not on sets. 
%%% The result list is made from all of the elements of L1 followed by every element from L2 if L1 does not have such element. The order of the elements in both lists is maintained. If L1 originally has duplicates, the function does not remove duplicates in the same list. However, it does not duplicate the elements while copying if two elements from different input lists compare equal.
%%% L1 must be a list of elements. Element must not be a list. Therefore, A list of lists is not expected.
%%% L2 must be a list of elements. Element must not be a list. Therefore, a list of lists is not expected. 

umerge(L1, L2) -> 
    umerge(L1, L2, L1).

umerge(_, [], U) -> 
    U;
umerge(L1, [Hl2|Tl2], U) ->   
    case exists(Hl2, L1) of 
	    false ->
	    	umerge(L1, Tl2, lists:append(U, [Hl2]));
	    true ->
	   	 umerge(L1, Tl2, U)
	   end.

exists(_, []) -> 
    false;
exists(X, [X|_]) -> 
    true;
exists(X, [_|T]) -> 
    exists(X, T).
