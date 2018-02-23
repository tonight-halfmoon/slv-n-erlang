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
%-record(anode, {key1 :: integer(), key2 = erlang:timestamp(), data = erlang:term()}).

-opaque linked_list() :: #lns{}.

-type node@() :: term().

-define(initial_key, 1).
-define(increment_key, 1).
-define(decrement_key, -1).

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

nth(_N, _I, _Tab, '$end_of_table') ->
    not_found;
nth(N, N, Tab, Key) ->
    {Key, ets:lookup_element(Tab, Key, 2)};
nth(N, I, Tab, Key) ->
    nth(N, I + 1, Tab, ets:next(Tab, Key)).

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

%% key_uniform(State) ->
%%     case rand:uniform(State) of
%% 	{0.0, NewState} ->
%% 	    key_uniform(NewState);
%% 	Result ->
%% 	    Result
%%     end.
