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

-export([rs/0]).

rs() ->
    eunit:test([lnknds], [verbose, {report, {eunit_surefire, [{dir, "."}]}}]).

api_new_test_() ->
    {
      "When function `new/0` is invoked, then a new empty Linked List must be returned",
      ?_assertEqual({linked_list ,[]}, lns:new())
    }.

api_push_test_() ->
    {
      "When function `push/2` is invoked on an empty Linked List and some data input, then it must compose a new node containing the data provided and return a linked list which has the new node",
      {
	setup,
	fun() -> lns:new() end,
	fun(Linknodes) -> [?_assertMatch({linked_list,[{node,Timestamp,
					 {data,v1},
					 {time_visited,undefined}}]} when Timestamp > 0, lns:push(Linknodes, 'v1'))
		   ]
	end
      }
    }.
