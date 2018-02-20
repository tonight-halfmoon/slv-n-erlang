%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% On the Feb 18th 2018 

-module(lns_tests).

-export([run_suite/0,
	 setup/1,
	 setup/2,
	 after_each/1]).

-include_lib("eunit/include/eunit.hrl").

run_suite() ->
    eunit:test([lns], [verbose, {report, {eunit_surefire, [{dir, "."}]}}]).

after_each(_) ->
    true.

setup(To) ->
    lns:from_list(setup_list(1, To)).

setup(From, To) ->
    lns:from_list(setup_list(From, To)).

setup_list(From, To) ->
    [list_to_atom(lists:concat(['v', X])) || X <- lists:seq(From, To)].

api_from_list_test_() ->
    {
      "When Client asks for a Linked List providing a Erlang list of elements, then a Linked List must be returned each node having the value from the next index in the input list",
      ?_assertEqual({lns,{node,v1,{node,v2,{node,v3,nil,{time_visited,0}},{time_visited,0}},{time_visited,0}}}, lns:from_list(['v1', 'v2', 'v3']))
    }.

api_to_list_test_() ->
    {
      "When Client asks for a list providing a Linked List, then the two properties of each node `value` and `time_visited` must be collected and returned as a Erlang List",
      ?_assertMatch([{v1, T1},
                      {v2, T2},
                      {v3, T3}] when T1 > 0; T2 > 0; T3 > 0, lns:to_list({lns,{node,v1,{node,v2,{node,v3,nil,{time_visited,0}},{time_visited,0}},{time_visited,0}}}))
    }.

api_tail_test_() ->
    {
      "When a given Linked List has one node then the tail is the head",
      {
	setup,
	fun () -> lns:append(lns:new(), 'v1') end,
	fun(Lns) ->
		[?_assertEqual({node, v1, nil, {time_visited, 0}}, lns:tail(Lns))]
	end
      }
    }.

api_visit_all_test_() ->
    {
      "When a given Linked List is visited, then all its nodes must be updated on its property `timestamp`",
      {
	setup,
	fun() -> setup(3) end,
	fun(Lns) ->
		[?_assertMatch({lns,{node,v1, {node,v2, {node,v3,nil,{time_visited,T1}},{time_visited,T2}}, {time_visited,T3}}} when T1 > 0; T2 > 0; T3 > 0, lns:visit_all(Lns))]
	end
      }
    }.

api_pop_test_() ->
    {
      "When function `pop` is invoked on a given linked list, then it must extract the data from the head, delete the node, advance the head pointer to point at the next node in line",
      {
	setup,
	fun() -> setup(3) end,
	fun(Lns) ->
		[?_assertEqual({v1,{lns,{node,v2,{node,v3,nil,{time_visited,0}},{time_visited,0}}}}, lns:pop(Lns))]
	end
      }
    }.

api_nth_test_() ->
    N = 923,
    {
      "When function `nth` is invoked on a given linked list, then it must return the data in the nth node",
      {
	setup,
	fun() -> setup(0, 1000) end,
	fun(Lns) ->
		[?_assertEqual(list_to_atom(lists:concat(['v', N])), lns:nth(N, Lns))]
	end
      }
    }.

api_nth_0_test_() ->
    N = 0,
    {
      "When function `nth` is invoked on a given linked list, then it must return the data in the nth node",
      {
	setup,
	fun() -> setup(0, 1000) end,
	fun(Lns) ->
		[?_assertEqual(list_to_atom(lists:concat(['v', N])), lns:nth(N, Lns))]
	end
      }
    }.

api_extend_test_() ->
    {
      "When function `extend` is invoked on two linked lists given, then it must append the second linked list onto the end of the first linked list",
      {
	setup,
	fun() -> [setup(1, 40), setup(41, 80), setup(1, 80)] end,
	fun([Lns1, Lns2, Expected]) ->
		[?_assertEqual(Expected, lns:extend(Lns1, Lns2))]
	end
      }
    }.
