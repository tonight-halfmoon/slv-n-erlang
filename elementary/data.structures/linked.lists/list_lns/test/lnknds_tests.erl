%%%-------------------------------------------------------------------
%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% On the 24th of February 2018
%%%-------------------------------------------------------------------
-module(lnknds_tests).

-include_lib("eunit/include/eunit.hrl").

-export([eval_testcases/0]).

-define(module, lnknds).

eval_testcases() ->
    eunit:test([lnknds], [verbose, {report, {eunit_surefire, [{dir, "."}]}}]).

api_new_test_() ->
    {
      "When function `new/0` is invoked, then a new empty Linked List must be returned",
      ?_assertEqual({linked_list ,[]}, ?module:new())
    }.

api_push_test_() ->
    {
      "When function `push/2` is invoked on an empty Linked List and some data input, then it must compose a new node containing the data provided and return a linked list which has the new node",
      {
	setup,
	fun() -> ?module:new() end,
	fun(Linknodes) -> [?_assertMatch({linked_list,[{node, Timestamp,
					 {data,v1},
					 {time_visited,undefined}}]} when Timestamp > 0, ?module:push(Linknodes, 'v1'))
		   ]
	end
      }
    }.

api_from_list_test_() ->
    {
      "When function `from_list/1` is invoked on a non-empty Erlang type `list()`, then it must return a Linked List having the same size of the source list, each node has the data from the next source list's index",
      {
	setup,
	fun() -> [list_to_atom(lists:concat(['v', X])) || X <- lists:seq(1,3)] end,
	fun(SourceList) -> [?_assertMatch({linked_list, [{node, Tc1,{data, v1}, _Tv1},
							 {node, Tc2, {data, v2}, _Tv2},
							 {node, Tc3, {data, v3}, _Tv3}]}
					  when Tc1 < Tc2; Tc2 < Tc3; Tc1 > 0,
					       ?module:from_list(SourceList))
			   ]
	end
      }
    }.

api_from_list_assert_node_creation_timestamp_test_() ->
    {
      "When function `from_list/1` is invoked on a non-empty Erlang type `list()`, then it must return a Linked List having the same size of the source list, each node has the data from the next source list's index",
      {
	setup,
	fun() -> SourceList = [list_to_atom(lists:concat(['v', X])) || X <- lists:seq(1,3)],
		 [?module:from_list(SourceList), os:system_time()] 
	end,
	fun([ActualLinkedList, SystemTime]) ->
		[?_assertMatch({linked_list, [{node, Tc1,{data, v1}, _Tv1},
					      {node, Tc2, {data, v2}, _Tv2},
					      {node, Tc3, {data, v3}, _Tv3}]}
			       when Tc1 < Tc2; Tc2 < Tc3; Tc1 > 1; Tc3 < SystemTime, ActualLinkedList)
		]
	end
      }
    }.

api_to_list_test_() ->
    {
      "When function `to_list/1` is invoked on a Linked List, then it must return a Erlang type `list()` having the same size of the source Linked List, each index has a pair of `Key` and `Data` from the next source Linked List's Node",
      {
	setup,
	fun() -> SourceList = [list_to_atom(lists:concat(['v', X])) || X <- lists:seq(1,3)],
		 ?module:from_list(SourceList)
	end,
	fun(SourceLinkedList) ->
		[?_assertMatch([{_Tc1, v1}, {_Tc2, v2}, {_Tc3, v3}], ?module:to_list(SourceLinkedList))]
	end
      }
    }.

api_head_test_() ->
    {
      "When function `head/1` is invoked on a Linked List, then it must return the first node in the Linked List provided",
      {
	setup,
	fun() -> SourceList = [list_to_atom(lists:concat(['v', X])) || X <- lists:seq(1,3)],
		 ?module:from_list(SourceList)
	end,
	fun(SourceLinkedList) ->
		[?_assertMatch({node,_Tc, {data, v1}, {time_visited, undefined}}, ?module:head(SourceLinkedList))]
	end
      }
    }.

api_nth_test_() ->
    {
      "When function `nth/2` is invoked on a Linked List and `N` integer value provided > 1 and < the size of the Linked List input, then it must return the Nth node in the input Linked List",
      {
	setup,
	fun() -> SourceList = [list_to_atom(lists:concat(['v', X])) || X <- lists:seq(1,3)],
		 ?module:from_list(SourceList)
	end,
	fun(SourceLinkedList) ->
		[?_assertMatch({node,_Tc, {data, v2}, {time_visited, undefined}}, ?module:nth(2, SourceLinkedList))]
	end
      }
    }.

api_nth_with_N_greater_than_size_test_() ->
    {
      "When function `nth/2` is invoked on a Linked List and `N` integer value provided > the size of the input Linked List, then it must return message `outside+`",
      {
	setup,
	fun() -> SourceList = [list_to_atom(lists:concat(['v', X])) || X <- lists:seq(1,3)],
		 ?module:from_list(SourceList)
	end,
	fun(SourceLinkedList) ->
		[?_assertMatch('outisde+', ?module:nth(4, SourceLinkedList))]
	end
      }
    }.

api_tail_test_() ->
    {
      "When function `tail/1` is invoked on a Linked List, then it must return the last node in the Linked List provided",
      {
	setup,
	fun() -> SourceList = [list_to_atom(lists:concat(['v', X])) || X <- lists:seq(1,3)],
		 ?module:from_list(SourceList)
	end,
	fun(SourceLinkedList) ->
		[?_assertMatch({node, _Tc, {data, V3}, _Tv} when V3 == 'v3', ?module:tail(SourceLinkedList))]
	end
      }
    }.

api_append_test_() ->
    {
      "When function `append/2` is invoked on a Linked List and data value provided, then it must append to the last a new node having the data provided to the Linked List and return the updated Linked List",
      {
	setup,
	fun() -> SourceList = [list_to_atom(lists:concat(['v', X])) || X <- lists:seq(1,3)],
		 ?module:from_list(SourceList)
	end,
	fun(SourceLinkedList) ->
		[?_assertMatch({linked_list,[_Node1, _Node2, _Node3, {node, _Tc4, {data, V4}, _Tv4}]} when V4 ==  'v4', ?module:append(SourceLinkedList, 'v4'))]
	end
      }
    }.

api_append_on_empty_linked_list_test_() ->
    {
      "When function `append/2` is invoked on a empty Linked List and data value provided, then it must append a new node having the data provided to the Linked List and return the updated Linked List with the new node appended to be the first node",
      {
	setup,
	fun() -> ?module:new() end,
	fun(LinkedList) ->
		[?_assertMatch({linked_list,[{node, _Tcx, {data, VX}, _Tvx}]} when VX ==  'vx', ?module:append(LinkedList, 'vx'))]
	end
      }
    }.

api_insert_test_() ->
    {
      "When function `insert/3` is invoked on a Linked List, Nth integer and data value provided, then it must insert a new node having the data provided in the Linked List to be the Nth node, and return the updated Linked List",
      {
	setup,
	fun() -> SourceList = [list_to_atom(lists:concat(['v', X])) || X <- lists:seq(1,3)],
		 ?module:from_list(SourceList)
	end,
	fun(LinkedList) ->
		[?_assertMatch({linked_list,[_Node1, {node, _Tc2, {data, V2}, _Tv2}, _Node2, _Node3]} when V2 ==  'v2x', ?module:insert(LinkedList, 2, 'v2x'))]
	end
      }
    }.

api_pop_test_() ->
    {
      "When function `pop/1` is invoked on a Linked List, then it must remove the head of the Linked List, and return a tuple of the head node and the updated Linked List",
      {
	setup,
	fun() -> SourceList = [list_to_atom(lists:concat(['v', X])) || X <- lists:seq(1,3)],
		 ?module:from_list(SourceList)
	end,
	fun(LinkedList) ->
		[?_assertMatch({{node, _Tc1, {data, V1}, _Tv1}, {linked_list,[_Node2, _Node3]}} when V1 ==  'v1', ?module:pop(LinkedList))]
	end
      }
    }.

api_find_test_() ->
    {
      "When function `find/2` is invoked on a data value and a Linked List, then it must search the Linked List and return the Node that have the data in key `data`",
      {
	setup,
	fun() -> SourceList = [list_to_atom(lists:concat(['v', X])) || X <- lists:seq(1,1000)],
		 ?module:from_list(SourceList)
	end,
	fun(LinkedList) ->
		[?_assertMatch({node, _Tc, {data, V}, _Tv} when V ==  'v349', ?module:find('v349', LinkedList))]
	end
      }
    }.

api_find_when_not_found_test_() ->
    {
      "When function `find/2` is invoked on a data value and a Linked List, then it must search the Linked List and return false if the data in key `data` not found",
      {
	setup,
	fun() -> SourceList = [list_to_atom(lists:concat(['v', X])) || X <- lists:seq(1,1000)],
		 ?module:from_list(SourceList)
	end,
	fun(LinkedList) ->
		[?_assertEqual(false, ?module:find('v3490', LinkedList))]
	end
      }
    }.

api_remove_test_() ->
    {
      "When function `remove/2` is invoked on a data value and a Linked List, then it must remove the first node whose data value compares equal to `Data` value provied and return the updated Linked List",
      {
	setup,
	fun() -> SourceList = [list_to_atom(lists:concat(['v', X])) || X <- lists:seq(1,3)],
		 ?module:from_list(SourceList)
	end,
	fun(LinkedList) ->
		[?_assertMatch({linked_list,[{node, _Tc1,{data,v1},{time_visited,undefined}},{node, _Tc3,{data,v3},{time_visited,undefined}}]}, ?module:remove('v2', LinkedList))]
	end
      }
    }.

api_take_test_() ->
    {
      "Function `take/2` searches the linked list `LinkedList1` for a node whose data element compares equal to `Data` argument provided. Returns `Node, LinkedList2}` if such a node is founded, otherwise false. Node is defined as a `#node{}`. `LinkedList2` is a copy of `LinkedList1` where the first occurance of `Node` has been removed.",
      {
	setup,
	fun() -> SourceList = [list_to_atom(lists:concat(['v', X])) || X <- lists:seq(1,3)],
		 ?module:from_list(SourceList)
	end,
	fun(LinkedList1) ->
		[?_assertMatch({{node, _Tcn, {data, 'v2'}, {time_visited, undefined}}, {linked_list,[{node, _Tc1,{data,v1},{time_visited,undefined}},{node, _Tc3,{data,v3},{time_visited,undefined}}]}}, ?module:take('v2', LinkedList1))]
	end
      }
    }.

api_take_when_not_found_test_() ->
    {
      "Function `take/2` searches the linked list `LinkedList1` for a node whose data element compares equal to `Data` argument provided. Returns `false` when such a node is not found.",
      {
	setup,
	fun() -> SourceList = [list_to_atom(lists:concat(['v', X])) || X <- lists:seq(1,3)],
		 ?module:from_list(SourceList)
	end,
	fun(LinkedList1) ->
		[?_assertMatch(false, ?module:take('v1--', LinkedList1))]
	end
      }
    }.

