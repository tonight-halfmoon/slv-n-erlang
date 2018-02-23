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
	 pop/1,
	 insert/3]).

-export_type([linked_list/0]).

-record(lns, {ntab = notable :: ets:tab()}).
%-record(anode, {key1 :: integer(), key2 = erlang:timestamp(), data = erlang:term()}).

-opaque linked_list() :: #lns{}.

-type node@() :: term().

-define(initial_key, 1).
-define(increment_key, 576460752303423487).
-define(decrement_key, -134217727).

%%%===================================================================
%%%  API
%%%===================================================================

-spec new() -> linked_list().

new() -> #lns{ntab = ets:new(ntab, [ordered_set])}.

-spec append(linked_list(), erlang:term()) -> linked_list().

append(LL, Data) ->
    append(LL#lns.ntab, ets:last(LL#lns.ntab), Data).

-spec push(linked_list(), atom()) -> linked_list().

push(LL, Data) ->
    First = ets:first(LL#lns.ntab),
    case First of
	'$end_of_table' ->
	    ets:insert_new(LL#lns.ntab, {?initial_key, Data});
	Key when is_integer(Key) ->
	    ets:insert_new(LL#lns.ntab, {First + ?decrement_key, Data})
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
    nth(N, 0, LL#lns.ntab, ets:first(LL#lns.ntab)).

info(LL) ->
    ets:info(LL#lns.ntab).

-spec pop(linked_list()) -> {node@(), linked_list()}.

pop(LL) ->
    Hkey = ets:first(LL#lns.ntab),
    ets:take(LL#lns.ntab, Hkey),
    {lookup(LL, Hkey), LL}.

-spec insert(LL :: linked_list(), Nth :: integer(), Data :: erlang:term()) -> linked_list().
 
insert(LL, Nth, Data) ->
    insert(LL#lns.ntab, ets:first(LL#lns.ntab), Nth, 1, Data).

%%%===================================================================
%%% Internal Functions
%%% @private
%%%===================================================================

append(Tab, '$end_of_table', Data) ->
    ets:insert_new(Tab, {?initial_key, Data});
append(Tab, LastKey, Data) when is_integer(LastKey) ->
    ets:insert_new(Tab, {LastKey + ?increment_key, Data}).

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

nth(_N, _I, _Tab, '$end_of_table') ->
    not_found;
nth(N, N, Tab, Key) ->
    {Key, ets:lookup_element(Tab, Key, 2)};
nth(N, I, Tab, Key) ->
    nth(N, I + 1, Tab, ets:next(Tab, Key)).

key_in_between(KeyPrev, KeyNext) ->
    KeyNext - KeyPrev bsr 1 + KeyPrev.

insert(Tab, '$end_of_table', _N, 1, Data) ->
    append(Tab, '$end_of_table', Data);
insert(Tab, '$end_of_table', _N, _I, Data) ->
    append(Tab, ets:last(Tab), Data);
insert(Tab, KeyNext, N, N, Data) ->
    KeyPrev = ets:prev(Tab, KeyNext),
    NewKey = key_in_between(KeyPrev, KeyNext),
    ets:insert_new(Tab, {NewKey, Data});
insert(Tab, Key, N, I, Data) ->
    insert(Tab, ets:next(Tab, Key), N, I + 1, Data).
