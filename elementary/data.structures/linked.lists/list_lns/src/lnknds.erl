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
	 pop/1,
	 find/2,
	 remove/2, take/2
	]).

-export_type([linknodes/0]).

-record(time_visited, {timestamp :: erlang:timestmap()}).
-record(data, {value :: term()}).
-record(node, {key :: integer(), data = #data{}, time_visited = #time_visited{}}).
-record(linked_list, {list = nolist :: [#node{}] | []}).

-opaque linknodes() :: #linked_list{}.

%%%===================================================================
%%%  API
%%%===================================================================

-spec new() -> linknodes().

new() -> #linked_list{list = []}.

-spec push(Linknodes :: linknodes(), Data :: term()) -> Result :: linknodes().

push(Linknodes, Data) ->
    Linknodes#linked_list{list = insert_new(Linknodes#linked_list.list, Data, fun push/3)}.

-spec append(Linknodes :: linknodes(), Data :: term()) -> Result :: linknodes().

append(Linknodes, Data) ->
    Linknodes#linked_list{list = insert_new(Linknodes#linked_list.list, Data, fun append/3)}.

-spec insert(LinkedList :: linknodes(), Nth :: integer(), Data :: term()) -> linknodes().

insert(Linknodes, Nth, Data) ->
    Linknodes#linked_list{list = insert_new(Linknodes#linked_list.list, {Nth, Data}, fun append/3)}.

-spec from_list([T, ...] | []) -> linknodes() when
      T :: term().

from_list(L) ->
    Linknodes = new(),
    Linknodes#linked_list{list = from_list(lists:reverse(L), Linknodes#linked_list.list)}.

-spec to_list(linknodes()) -> [Tuple, ...] when
      Tuple :: {integer(), term()}.

to_list(Linknodes) ->
    to_tuplelist(Linknodes#linked_list.list, []).

-spec head(Linknodes :: linknodes()) -> #node{}.

head(Linknodes) ->
    nth(1, Linknodes).

-spec nth(N :: integer(), linknodes()) -> term().

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

-spec find(Data :: term(), linknodes()) -> Result :: #node{} | 'false'.

find(Data, Linknodes) ->
    case lists:keyfind(Data, 2, to_list(Linknodes)) of
	{Tc, V} ->
	    #node{key = Tc, data = #data{value = V}};
	{Tc, V, Tv} ->
	    #node{key = Tc, data = #data{value = V}, time_visited = #time_visited{timestamp = Tv}};
	false ->
	    'false'
    end.

-spec remove(Data :: term(), linknodes()) -> linknodes().

remove(Data, Linknodes) ->
    Linknodes#linked_list{list = from_tuplelist(lists:reverse(remove(Data, 2, to_list(Linknodes))))}.

-spec take(Data, Linknodes) -> {Taken, Linknodes} | 'false' when
      Data :: term(),
      Taken :: #node{},
      Linknodes :: linknodes().

take(Data, Linknodes) ->
    case take(Data, 2, to_list(Linknodes)) of
	{Tuple, UpdatedLinknodes} ->
	    {from_tuple(Tuple), Linknodes#linked_list{list = from_tuplelist(lists:reverse(UpdatedLinknodes))}};
	false ->
	    'false'
    end.

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

-spec from_list(SourceList, Result) -> Result when
      SourceList :: [T, ...],
      Result :: linknodes(),
      T :: term().

from_list([], Result) ->
    Result;
from_list([H|T], Result) ->
    from_list(T, insert_new(Result, H, fun push/3)).

-spec push(L :: erlang:list() | [], Data :: term(), NewNodeFun :: fun()) -> erlang:list().

push(L, Data, NewNodeFun) ->
    [NewNodeFun(Data)|L].

-spec new_node(term()) -> #node{}.

new_node(Data) ->
    #node{key = key_uniform(os:system_time()), data = #data{value = Data}}.

-spec new_node(Key, Data, Tv) -> #node{} when
      Key :: erlang:timestamp(),
      Data :: term(),
      Tv :: erlang:timestamp().

new_node(Key, Data, Tv) ->
    #node{key = Key, data = #data{value = Data}, time_visited = #time_visited{timestamp = Tv}}.

-spec from_tuple(Tuple) -> Node when
      Tuple :: tuple(),
      Node :: #node{}.

from_tuple(Tuple) ->
    case Tuple of
	{Key, Data, Tv} ->
	    new_node(Key, Data, Tv);
	{Key, Data} ->
	    new_node(Key, Data, undefined)
    end.

-spec to_tuplelist(ListOfNodes, Result) -> Result when
      ListOfNodes :: [#node{}, ...],
      Result :: [{Key, Data}, ...], 
      Key :: erlang:timestamp(), 
      Data :: term().

to_tuplelist([], Result) ->
    lists:reverse(Result);
to_tuplelist(_ListOfNodes = [_H = #node{key = Key, data = #data{value = Data}, time_visited = _Tc}|T], Result) ->
    to_tuplelist(T, [{Key, Data}|Result]).

-spec insert_new(L, Object, Fun) -> Result when
      L :: erlang:list(),
      Object :: {Nth, Data} 
	      | term(),
      Nth :: integer(),
      Data :: term(),
      Fun :: fun(),
      Result :: [T, ...],
      T :: #node{}.

insert_new(L, Object, InsertMethodFun) ->
    InsertMethodFun(L, Object, fun new_node/1).

-spec append(L :: erlang:list(), {Nth :: integer(), Data :: term()}, fun()) -> erlang:list();
	    (L :: erlang:list(), Data :: term(), fun()) -> erlang:list().

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

from_tuplelist(L) ->
    from_tuplelist(L, []).

from_tuplelist([], Result) ->
    Result;
from_tuplelist([H|T], Result) ->
    from_tuplelist(T, [from_tuple(H)|Result]).

-spec remove(Key :: term(), N :: integer(), L :: erlang:list()) -> Result :: erlang:list().

remove(Key, N, L) ->
    lists:keydelete(Key, N, L).

-spec take(Key :: term(), N :: integer(), L :: erlang:lest()) -> {Taken, L2} | 'false' when
      Taken :: tuple(),
      L2 :: erlang:list().

take(Key, N, L) ->
    case lists:keytake(Key, N, L) of
	{value, Tuple, UpdatedLinknodes} ->
	    {Tuple, UpdatedLinknodes};
	false ->
	    'false'
    end.

