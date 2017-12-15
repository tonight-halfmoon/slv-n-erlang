%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2017, 
%%% @doc
%%%
%%% @end
%%% On the 27th of November 2017

-module(node_lib).
-export([start_node/3]).

-spec start_node(atom(), atom(), atom()) -> {atom(), pid()}.  % NameType = longnames|shortnames
start_node(Name, Fully_qualified_name, NameType) ->
    case net_kernel:start([Name, NameType]) of
	{ok, Pid} ->
	    io:format("A distributed node ~p started with Pid ~p~n", [Fully_qualified_name, Pid]),
	    {Fully_qualified_name, Pid};
	{error, {already_started, Pid}} ->
	    io:format("The distributed node ~p already started with Pid ~p~n", [Fully_qualified_name, Pid]),
	    {Fully_qualified_name, Pid};
	M ->
	    io:format("Failed to start a distributed node. ~p~n", [M])
    end.
