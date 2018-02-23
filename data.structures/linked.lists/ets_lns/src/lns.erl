%%%-------------------------------------------------------------------
%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% On the 20th of February 2018
%%%-------------------------------------------------------------------
-module(lns).

-export([new/0, push/2,
	 append/2,
	 from_list/1,
	 to_list/1,
	 head/1,
	 tail/1,
	 nth/2,
	 info/1,
	 pop/1]).

-export_type([linked_list/0]).

-record(lns, {ntab = notable :: ets:tab()}).

-opaque linked_list() :: #lns{}.

-type node@() :: term().

-define(initial_key, 1).
-define(increment_key, 1).

%%%===================================================================
%%%  API
%%%===================================================================

-spec new() -> linked_list().

new() -> #lns{ntab = ets:new(ntab, [ordered_set])}.

append(LL, Data) ->
    Last = ets:last(LL#lns.ntab),
    case Last of
	'$end_of_table' ->
	    ets:insert_new(LL#lns.ntab, {?initial_key, Data});
	Key when is_integer(Key) ->
	    ets:insert_new(LL#lns.ntab, {Last + ?increment_key, Data})
    end.

-spec push(linked_list(), atom()) -> linked_list().

push(LL, Data) ->
    First = ets:first(LL#lns.ntab),
    case First of
	'$end_of_table' ->
	    ets:insert_new(LL#lns.ntab, {?initial_key, Data});
	Key when is_integer(Key) ->
	    ets:insert_new(LL#lns.ntab, {First - ?increment_key, Data})
    end.

-spec from_list(L) -> LL when
      L :: list() | nil(),
      LL :: linked_list().

from_list(X) when not is_list(X) -> type_list_expected;
from_list(L) -> prepend_all(L, new()).

-spec to_list(LL) -> L when
      LL :: linked_list(),
      L :: [node@()].

to_list(LL) -> ets:tab2list(LL#lns.ntab).

-spec head(LL :: linked_list()) -> node@().

head(LL) ->
    [Head] = ets:lookup(LL#lns.ntab, ets:first(LL#lns.ntab)),
    Head.

-spec tail(LL :: linked_list()) -> node@().

tail(LL) ->
    [Tail] = ets:lookup(LL#lns.ntab, ets:last(LL#lns.ntab)),
    Tail.

-spec nth(N :: integer(), linked_list()) -> node@().

nth(N, LL) ->
    case ets:first(LL#lns.ntab) of
	'$end_of_table' ->
	    empty_linked_list;
	Key when abs(Key) > Key ->
	    lookup(LL, N + Key);
	Key when Key > 1 ->
	    lookup(LL, N + Key + 1);
	Key when Key =:= 1 ->
	    lookup(LL, N)
    end.

info(LL) ->
    ets:info(LL#lns.ntab).

-spec pop(linked_list()) -> {node@(), linked_list()}.

pop(LL) ->
    Hkey = ets:first(LL#lns.ntab),
    ets:take(LL#lns.ntab, Hkey),
    {lookup(LL, Hkey), LL}.

%%%===================================================================
%%% Internal Functions
%%% @private
%%%===================================================================

lookup(LL, Key) ->
    case ets:lookup(LL#lns.ntab, Key) of
	[] ->
	    not_found;
	[Nth] ->
	    Nth
    end.

prepend_all([], LL) ->
    LL;
prepend_all([H|T], LL) ->
    push(LL, H),
    prepend_all(T, LL).
