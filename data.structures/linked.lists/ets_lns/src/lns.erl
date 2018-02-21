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
	 info/1]).

-export_type([linked_list/0]).

-record(lns, {ntab = notable :: ets:tab()}).

-opaque linked_list() :: #lns{}.

-define(initial_key, 1).
-define(increment_key, 1).

-spec new() -> linked_list().

new() ->
    N = ets:new(ntab, [ordered_set]),
    #lns{ntab = N}.

append(LL, Data) ->
    Last = ets:last(LL#lns.ntab),
    case Last of
	'$end_of_table' ->
	    ets:insert_new(LL#lns.ntab, {?initial_key, Data});
	Key when is_integer(Key) ->
	    ets:insert_new(LL#lns.ntab, {Last + ?increment_key, Data})
    end.

push(LL, Data) ->
    First = ets:first(LL#lns.ntab),
    case First of
	'$end_of_table' ->
	    ets:insert_new(LL#lns.ntab, {?initial_key, Data});
	Key when is_integer(Key) ->
	    ets:insert_new(LL#lns.ntab, {First - ?increment_key, Data})
    end.

from_list(_L) ->
    true.

to_list(_LL) ->
    true.

head(LL) ->
    [Head] = ets:lookup(LL#lns.ntab, ets:first(LL#lns.ntab)),
    Head.

tail(LL) ->
    [Tail] = ets:lookup(LL#lns.ntab, ets:last(LL#lns.ntab)),
    Tail.

nth(N, LL) ->
    case ets:first(LL#lns.ntab) of
	'$end_of_table' ->
	    empty_linked_list;
	Key when abs(Key) > Key ->
	    lookup(LL, N + Key);
	_Key ->
	    lookup(LL, N)
    end.

lookup(LL, Key) ->
    case ets:lookup(LL#lns.ntab, Key) of
	[] ->
	    not_found;
	[Nth] ->
	    Nth
    end.

info(LL) ->
    ets:info(LL#lns.ntab).
