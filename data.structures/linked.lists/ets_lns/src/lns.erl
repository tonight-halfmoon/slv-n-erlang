%%%-------------------------------------------------------------------
%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2018, 
%%% @doc
%%% 
%%% Capability to insert `Nth` node is `58 nodes` in between at max with `bsr 1`; given 
%%% -define(increment_key, 576460752303423487).
%%% -define(decrement_key, -134217727).
%%%
%%% @end
%%% On the 20th of February 2018
%%%-------------------------------------------------------------------
-module(lns).

-include_lib("eunit/include/eunit.hrl").

-export([new/0,
	 push/2, append/2, insert/3,
	 from_list/1,
	 to_list/1,
	 head/1, tail/1,
	 nth/2,
	 info/1,
	 pop/1]).

-export_type([linked_list/0]).

-record(lns, {ntab = notable :: ets:tab()}).

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
    case First = ets:first(LL#lns.ntab) of
	'$end_of_table' ->
	    append(LL, Data);
	First when First == Nth ->
	    cannot_replace_the_head; %% Tentative bug
	First when Nth < First ->
	    neglected;
	_ ->
	    insert(LL#lns.ntab, ets:first(LL#lns.ntab), Nth, 1, Data)
    end.

%%%===================================================================
%%% Internal Functions
%%% @private
%%%===================================================================

-spec append(linked_list(), '$end_of_table', Data :: erlang:term()) -> true | false
     ; (linked_list(), LastKey :: integer(), Data :: erlang:term()) -> true | false.
append(Tab, '$end_of_table', Data) -> %% assumes a fresh Linked List (empty LL). Tentative bug.
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

-spec key_in_between(KeyPrev :: integer(), KeyNext :: integer()) -> integer().
%% Tentative fatal error! Incomplete function definition
key_in_between(KeyPrev, KeyNext) when is_integer(KeyPrev) andalso is_integer(KeyNext) ->
    KeyNext - KeyPrev bsr 1 + KeyPrev.

insert(Tab, '$end_of_table', _N, 1, Data) ->
    append(Tab, '$end_of_table', Data);
insert(Tab, '$end_of_table', _N, _I, Data) ->
    append(Tab, ets:last(Tab), Data);
insert(Tab, KeyNext, N, N, Data) ->
    KeyPrev = ets:prev(Tab, KeyNext),
    NewKey = key_in_between(KeyPrev, KeyNext),
    case NewKey == KeyPrev of
	true ->
	    incapable;
	false ->
	    ets:insert_new(Tab, {NewKey, Data})
    end;
insert(Tab, Key, N, I, Data) ->
    insert(Tab, ets:next(Tab, Key), N, I + 1, Data).

%%%===================================================================
%%% Unit Tests on Internal Functions
%%% @private
%%%===================================================================

key_in_between_test() ->
    ?assertEqual(0, key_in_between(0,0)).

key_in_between_1_1_test() ->
    ?assertEqual(1, key_in_between(1,1)).

key_in_between_0_1_test() ->
    ?assertEqual(0, key_in_between(0, 1)).

key_in_between_2_test() ->
    ?assertEqual(0, key_in_between(1, -1)).

key_in_between_3_test() ->
    ?assertEqual(0, key_in_between(-1, 1)).

key_in_between_4_test() ->
    ?assertEqual(-1, key_in_between(-1, 0)).

key_in_between_5_test() ->
    ?assertEqual(0, key_in_between(-99, 99)).

key_in_between_6_test() ->
    ?assertEqual(98, key_in_between(97, 99)).

key_in_between_x_x_test() ->
    ?assertEqual(99, key_in_between(99, 99)).
