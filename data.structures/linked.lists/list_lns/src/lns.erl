%%%-------------------------------------------------------------------
%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% On the 23rd of February 2018
%%%-------------------------------------------------------------------
-module(lns).

-export([new/0]).

-export_type([linked_list/0]).

-record(time_visited, {timestamp :: erlang:timestmap()}).
-record(data, {value :: erlang:term()}).
-record(node, {key = 1 :: integer(), data = #data{}, time_visited = #time_visited{}}).
-record(lns, {ntab = notable :: [#node{}] | []}).

-opaque linked_list() :: #lns{}.

%%%===================================================================
%%%  API
%%%===================================================================

-spec new() -> linked_list().

new() -> #lns{ntab = [#node{}]}.
