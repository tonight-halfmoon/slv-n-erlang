%%%-------------------------------------------------------------------
%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% On the 23rd of February 2018
%%%-------------------------------------------------------------------
-module(lnknds).

-export([new/0, new_node/1,
	 push/2,
	 from_list/1, to_list/1,
	 head/1, nth/2
	]).

-export_type([linknodes/0]).

-record(time_created, {timestamp :: erlang:timestmap()}).
-record(data, {value :: erlang:term()}).
-record(node, {key = 1 :: integer(), data = #data{}, time_created = #time_created{}}).
-record(linked_list, {list = nolist :: [#node{}] | []}).

-opaque linknodes() :: #linked_list{}.

%%%===================================================================
%%%  API
%%%===================================================================

-spec new() -> linknodes().

new() -> #linked_list{list = []}.

-spec push(Linknodes :: linknodes(), Data :: erlang:term()) -> Result :: linknodes().

push(Linknodes, Data) ->
    Linknodes#linked_list{list = push(Linknodes#linked_list.list, Data, fun new_node/1)}.

-spec from_list(erlang:list() | []) -> linknodes().

from_list(L) ->
    from_list(lists:reverse(L), new()).

-spec to_list(linknodes()) -> erlang:list().

to_list(Linknodes) ->
    to_list(Linknodes#linked_list.list, []).

-spec head(Linknodes :: linknodes()) -> #node{}.

head(Linknodes) ->
    nth(1, Linknodes).

-spec nth(N :: integer(), linknodes()) -> erlang:term().

nth(N, Linknodes) when N > length(Linknodes#linked_list.list) ->
    'outisde+';
nth(N, _Linknodes) when N < 1 ->
    'outside-';
nth(_, []) ->
    empty;
nth(N, Linknodes) ->
    lists:nth(N, Linknodes#linked_list.list).

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

from_list([], Linknodes) ->
    Linknodes;
from_list([H|T], Linknodes) ->
    from_list(T, push(Linknodes, H)).

-spec push(L :: erlang:list() | [], Data :: erlang:term(), NewNodeFun :: fun()) -> erlang:list().

push(L, Data, NewNodeFun) ->
    [NewNodeFun(Data)|L].

-spec new_node(erlang:term()) -> #node{}.

new_node(Data) ->
    #node{key = key_uniform(os:system_time()), data = #data{value = Data}}.

-spec to_list(ListOfNodes :: erlang:list(), Result :: erlang:list()) -> Result :: erlang:list().

to_list([], Result) ->
    lists:reverse(Result);
to_list(_ListOfNodes = [_H = #node{key = Key, data = #data{value = Data}, time_created = _Tc}|T], Result) ->
    to_list(T, [{Key, Data}|Result]).
