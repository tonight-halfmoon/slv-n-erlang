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
	fun(Linknodes) -> [?_assertMatch({linked_list,[{node,Timestamp,
					 {data,v1},
					 {time_created,undefined}}]} when Timestamp > 0, ?module:push(Linknodes, 'v1'))
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
	fun(SourceList) -> [?_assertMatch({linked_list, [{node, Ts1,{data, v1}, _Tc1},
							 {node, Ts2, {data, v2}, _Tc2},
							 {node, Ts3, {data, v3}, _Tc3}]}
					  when Ts1 < Ts2; Ts2 < Ts3; Ts1 > 1,
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
		[?_assertMatch({linked_list, [{node, Ts1,{data, v1}, _Tc1},
					      {node, Ts2, {data, v2}, _Tc2},
					      {node, Ts3, {data, v3}, _Tc3}]}
			       when Ts1 < Ts2; Ts2 < Ts3; Ts1 > 1; Ts3 < SystemTime, ActualLinkedList)
		]
	end
      }
    }.

api_to_list_test_() ->
    {
      "When function `to_list/1` is invoked on Linked List, then it must return a Erlang type `list()` having the same size of the source Linked List, each index has a pair of `Key` and `Data` from the next source Linked List's Node",
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
