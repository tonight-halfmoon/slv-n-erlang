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
	 push/2, append/2, insert/3,
	 from_list/1, to_list/1,
	 head/1, nth/2, tail/1,
	 pop/1
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
    Linknodes#linked_list{list = insert_new(Linknodes#linked_list.list, Data, fun push/3)}.

-spec append(Linknodes :: linknodes(), Data :: erlang:term()) -> Result :: linknodes().

append(Linknodes, Data) ->
    Linknodes#linked_list{list = insert_new(Linknodes#linked_list.list, Data, fun append/3)}.

-spec insert(LinkedList :: linknodes(), Nth :: integer(), Data :: erlang:term()) -> linknodes().

insert(Linknodes, Nth, Data) ->
    Linknodes#linked_list{list = insert_new(Linknodes#linked_list.list, {Nth, Data}, fun append/3)}.

-spec from_list(erlang:list() | []) -> linknodes().

from_list(L) ->
    Linknodes = new(),
    Linknodes#linked_list{list = from_list(lists:reverse(L), Linknodes#linked_list.list)}.

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

-spec tail(LinkedList :: linknodes()) -> Last :: #node{}.

tail(Linknodes) ->
    lists:last(Linknodes#linked_list.list).

-spec pop(linknodes()) -> linknodes().

pop(Linknodes) ->
    [H|T] = Linknodes#linked_list.list,
    {H, Linknodes#linked_list{list = T}}.

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

-spec from_list(SourceList :: erlang:list(), Result :: erlang:list()) -> Resul :: erlang:list().

from_list([], Result) ->
    Result;
from_list([H|T], Result) ->
    from_list(T, insert_new(Result, H, fun push/3)).

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

-spec insert_new(L :: erlang:list(), {Nth :: integer(), Data :: erlang:term()} | erlang:term(), fun()) -> erlang:list().

insert_new(L, Object, InsertMethodFun) ->
    InsertMethodFun(L, Object, fun new_node/1).

-spec append(L :: erlang:list(), {Nth :: integer(), Data :: erlang:term()}, fun()) -> erlang:list();
	    (L :: erlang:list(), Data :: erlang:term(), fun()) -> erlang:list().

append(L, {Nth, Data}, NewNodeFun) ->
    case Nth of
	Nth when Nth < 1 ->
	    'outside-';
	Nth when Nth - 1 > length(L) ->
	    'outside+';
	Nth ->
	    {LH, LT} = lists:split(Nth - 1, L),
	    lists:append([LH, [NewNodeFun(Data)], LT])
    end;
append(L, Data, NewNodeFun) ->
    lists:append(L, [NewNodeFun(Data)]).
