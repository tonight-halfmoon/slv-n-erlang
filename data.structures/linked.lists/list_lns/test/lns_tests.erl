%%%-------------------------------------------------------------------
%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% On the 24th of February 2018
%%%-------------------------------------------------------------------
-module(lns_tests).

-include_lib("eunit/include/eunit.hrl").

api_new_test_() ->
    {
      "When function `new/0` is invoked, then a new empty Linked List must be returned",
      ?_assertEqual({lns,[{node,1,
			   {data,undefined},
			   {time_visited,undefined}}]}, lns:new())
    }.
