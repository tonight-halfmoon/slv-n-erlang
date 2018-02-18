%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% On the Feb 18th 2018 

-module(lns_tests).

-export([run_suite/0,
	after_each/1]).

-include_lib("eunit/include/eunit.hrl").

run_suite() ->
    eunit:test([lns], [verbose, {report, {eunit_surefire, [{dir, "."}]}}]).

after_each(_) ->
    true.

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
      "When a given Linked List is asked to be visited, then all its nodes must be updated on its property timestamp",
      {
	setup,
	fun () -> lns:from_list(['v1', 'v2', 'v3']) end,
	fun(Lns) ->
		[?_assertMatch({lns,{node,v1, {node,v2, {node,v3,nil,{time_visited,T1}},{time_visited,T2}}, {time_visited,T3}}} when T1 > 0; T2 > 0; T3 > 0, lns:visit_all(Lns))]
	end
      }
    }.
