%%% partial implementation for a Binary Seach Tree BST
%% 12th - 14th Feb 16; Jeddah, SA 
-module(tree3).
-export([insert/2, insert2/2, lookup/2, delete/2, delete2/2, sort/1, size_/1, empty/0]).
-include_lib("eunit/include/eunit.hrl").


%%%% Test Cases 

lookup_test() ->
    ?assertEqual(found, lookup(6, {6, empty(), empty()})).
lookup_4elem_tree_test() ->
    ?assertEqual(found, lookup(5, {5, {4, {3, nil,nil},nil}, {6, nil, nil}})).
lookup_12elem_tree_test() ->
    ?assertEqual(found, lookup(12, 
			       {11, 
				{10,
				 {9,
				  {8,
				   {7,
				    {6,
				     {5,
				      {4,
				       {3,
					{2,
					 {1, nil, nil},
					 nil},
					nil},
				       nil},
				      nil},
				     nil},
				    nil},
				   nil},
				  nil},
				 nil},
				{12, nil, nil}}
			      )).
lookup_noexist_elem_test() ->
    ?assertEqual(not_found, lookup(92, {88, {77,nil,nil},{102,nil,nil}})).
lookup_in_empty_tree_test() ->
    ?assertEqual(not_found, lookup(23, nil)).
size_test() ->
    ?assertEqual(0, size_(empty())).
size_12elem_tree_test() ->
    ?assertEqual(12, size_({5,{4,{1,nil,{3,nil,nil}},nil},{6,nil,{7,nil,{8,nil,{9,nil,{10,nil,{12,{11,nil,nil},{13,nil,nil}}}}}}}})).

insert_already_exist_elem_test() ->
    ?assertEqual(already_exist, insert(5, {5,nil,{6,nil,nil}})).

tree2list_test() ->
    ?assertEqual([1,3,4,5,6,7,8,9,10,11,12,13], sort({5,{4,{1,nil,{3,nil,nil}},nil},{6,nil,{7,nil,{8,nil,{9,nil,{10,nil,{12,{11,nil,nil},{13,nil,nil}}}}}}}})).
delete_test() ->
    ?assertEqual({7,{5,{4,nil,nil},nil},{8,nil,{9,nil,{10,nil,nil}}}}, delete2(6, {5,{4,nil,nil},{6,nil,{7,nil,{8,nil,{9,nil,{10,nil,nil}}}}}})).
delete_empty_tree_test() ->
    ?assertEqual(nothing_deleted, delete2(2, empty())).
delete_small_tree_test() ->
    ?assertEqual({5,nil,nil}, delete2(3, {5, {3,nil,nil},nil})).
delete_small2_tree_test() ->
    ?assertEqual({5,{3,nil,nil},nil}, delete2(6, {5, {3,nil,nil}, {6,nil,nil}})).
delete_small3_tree_test() ->
    ?assertEqual({5,nil,nil}, delete2(6, {5, nil, {6,nil,nil}})).
delete_small4_tree_test() ->
    ?assertEqual({6,nil,nil}, delete2(5, {5, nil, {6,nil,nil}})).
delete_small5_tree_test() ->
    ?assertEqual(nil, delete2(5, {5, nil, nil})).


%%% Data Structure Tree implementation

empty() -> nil.

insert(K, nil) ->  {K, empty(), empty()};
insert(K, {K,_, _}) -> already_exist;
insert(K,{Knext, Left, Right}) -> case ls(K, Knext) of
				   true ->
				       {Knext, insert(K, Left), Right};
				   false ->
				       {Knext, Left, insert(K,Right)}
			       end.
lookup(_, nil) -> not_found;
lookup(K, {K,_,_}) -> found;
lookup(K, {Nk, Left,Right}) -> case ls(K,Nk) of
					     true->
						 lookup(K, Left);
					     false ->
						 lookup(K, Right)
					 end.
size_(nil) -> 0;
size_({_, Left, Right}) -> size_(Left) + size_(Right) + 1.


sort(nil) -> [];
sort({K, Left, Right}) -> sort(Left) ++ [K] ++ sort(Right).

%% delete2 works but has a bug delete still has other bugs!!
delete(K, {K, nil, Right}) ->
    Right;
delete(K, {K, {KL, LL,_}, Right}) ->
    {KL, LL, Right};
delete (K, {Knext, Left, Right}) ->
    case ls(K, Knext) of 
	true ->
	    delete (K, Left);
	false ->
	    delete(K, Right)
    end;
delete (_, nil) ->
    already_nil.


delete2(K, Tree) -> delete2(K, Tree, nil).
delete2(_, nil,_) -> nothing_deleted;
delete2(K, {K, nil, Right}, Traversed) -> insert2(Traversed, Right);
delete2(K, {K, {KL, LL ,_}, Right}, Traversed) -> insert2({KL, LL, Right}, Traversed); %%buggy!!
delete2(K, {Knext, Left, Right}, Traversed) -> case ls(K, Knext) of 
						   true ->
						       delete2(K, Left, insert2(Traversed,{Knext, nil, Right}));
						   false ->
						       delete2(K, Right, insert2({Knext, Left, nil}, Traversed))
					       end.

insert2({K, Left, Right}, {KT,LT,RT}) -> case ls(K, KT ) of 
					     true ->
						 case LT =:= nil of true ->
							 {KT, {K, Left, Right}, RT};
						     false ->
							 insert2({K, Left, Right}, LT)
						 end;
					     false ->
						 case RT =:= nil of 
						     true ->
							 {KT, LT, {K, Left, Right}};
						     false ->
							 insert2({K, Left, Right}, RT)
						 end
					 end;
insert2({K, Left, Right}, nil) -> {K, Left, Right};
insert2(nil, Tree) -> Tree.

%%% Auxiliary functions 

ls(X,Y) -> X < Y.

