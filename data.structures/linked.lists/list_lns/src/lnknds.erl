%%%-------------------------------------------------------------------
%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% On the 23rd of February 2018
%%%-------------------------------------------------------------------
-module(lnknds).

-export([new/0,
	push/2]).

-export_type([linknodes/0]).

-record(time_visited, {timestamp :: erlang:timestmap()}).
-record(data, {value :: erlang:term()}).
-record(node, {key = 1 :: integer(), data = #data{}, time_visited = #time_visited{}}).
-record(linked_list, {nlist = nolist :: [#node{}] | []}).

-opaque linknodes() :: #linked_list{}.

%%%===================================================================
%%%  API
%%%===================================================================

-spec new() -> linknodes().

new() -> #linked_list{nlist = []}.

-spec push(LL :: linknodes(), Data :: erlang:term()) -> Result :: linknodes().

push(LL, Data) ->
    NewNode = #node{key = key_uniform(os:system_time()), data = #data{value = Data}},
    #linked_list{nlist = [NewNode|LL#linked_list.nlist]}.

%%%===================================================================
%%% Internal Functions
%%% @private
%%%===================================================================

key_uniform(State) ->
    case rand:uniform(State) of
	{0.0, NewState} ->
	    key_uniform(NewState);
	Result ->
	    Result
    end.
