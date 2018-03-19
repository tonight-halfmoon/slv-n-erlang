%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% On the 20th of February 2018
%%%-------------------------------------------------------------------

-module(lns_tests).

-export([run_suite/0, insert/2]).

-include_lib("eunit/include/eunit.hrl").

run_suite() ->
    eunit:test([lns], [verbose, {report, {eunit_surefire, [{dir, "."}]}}]).

setup(Lns) ->
    lns:push(Lns, 'v1'),
    lns:push(Lns, 'v2').

setup(LL, C) ->
    setup(LL, C, fun lns:push/2).

setup(Lns, C, Fun) ->
    setup(Lns, C + 1, 1, Fun).

setup(_Lns, C, C, _Fun) ->
    true;
setup(Lns, C, I, Fun) ->
    Fun(Lns, list_to_atom(lists:concat(['v', I]))),
    setup(Lns, C, I + 1, Fun).

api_new_test_() ->
    {
      "When function `new/0` is invoked, then it must return a new Linked List which is a ETS Tab of type `ordered_set`",
      {
	setup,
	fun() -> [lns:new(), ets:new(ntab, [ordered_set])]end,
	fun([ActualLinkedList, ExpectedNtab]) ->
		[?_assertEqual(ets:info(ExpectedNtab), lns:info(ActualLinkedList))]
	end
      }
    }.

api_push_test_() ->
    {
      "When function `push/2` is invoked on an empty Linked List, then the first node of the Linked List must be the node just got pushed",
      {
	setup,
	fun() -> Lns = lns:new(), lns:push(Lns, 'v1'), [Lns, {0, 'v1'}] end,
	fun([Lns, {_, Data}]) ->
		[?_assertMatch({Key, Data} when is_integer(Key), lns:head(Lns))]
	end
      }
    }.

api_push_2_test_() ->
    {
      "When function `push/2` is invoked on a Linked List, then the last element pushed is the head",
      {
	setup,
	fun() -> Lns = lns:new(), setup(Lns), lns:push(Lns, 'v3'), [Lns, {-268435453, 'v3'}] end,
	fun([Lns, {Key, Data}]) ->
		[?_assertEqual({Key, Data}, lns:head(Lns))]
	end
      }
    }.

api_nth_test_() ->
    {
      "When a client asks for the nth node of a Linked List, then the Nth node must be returned - setup with push/2",
      {
	setup,
	fun() -> Lns = lns:new(), setup(Lns, 3, fun lns:push/2), Lns end,
	fun(Lns) ->
		[?_assertEqual({-268435453, 'v3'}, lns:nth(0, Lns))]
	end
      }
    }.

api_nth_in_between_pop_test_() ->
    {
      "When a client asks for the nth node of a Linked List, then the Nth node must be returned - setup 3 nodes with push/2 and then pop/1",
      {
	setup,
	fun() -> Lns = lns:new(), setup(Lns, 3, fun lns:push/2), lns:pop(Lns), Lns end,
	fun(Lns) ->
		[?_assertEqual({-134217726, 'v2'}, lns:nth(0, Lns))]
	end
      }
    }.

api_nth_setup_with_append_test_() ->
    {
      "When a client asks for the nth node of a Linked List, then the Nth node must be returned - setup with append/2",
      {
	setup,
	fun() -> Lns = lns:new(), setup(Lns, 3, fun lns:append/2), Lns end,
	fun(Lns) ->
		[?_assertEqual({1152921504606846975, 'v3'}, lns:nth(2, Lns))]
	end
      }
    }.

api_to_list_test_() ->
    {
      "when function `to_list` is invoked on a Linked List, then it must return a list of nodes found in the input Linked List",
      {
	setup,
	fun() -> LL = lns:new(), setup(LL, 3), [LL, [{-268435453,'v3'}, {-134217726,'v2'}, {1,'v1'}]] end,
	fun([LL, Expected]) ->
		[?_assertEqual(Expected, lns:to_list(LL))]
	end
      }
    }.

api_from_list_test_() ->
    {
      "When function `from_list/1` is invoked with a Erlang list as input, then it must return a Linked Lists of nodes, each node having the next index value from the Erlang List input",
      {
	setup,
	fun() -> L = ['v1', 'v2', 'v3'], LL = lns:new(), setup(LL, 3), [L, LL] end,
	fun([L, Expected]) ->
		[?_assertEqual(lns:to_list(Expected), lns:to_list(lns:from_list(L)))]
	end
	
      }
    }.

api_pop_test_() ->
    {
      "When function `pop/1` is invoked on a given linked list, then it must extract the data from the head, delete the node, advance the head pointer to point at the next node in line",
      {
	setup,
	fun() -> LL = lns:new(), setup(LL, 3, fun lns:append/2),
		 [lns:pop(LL), v1] end,
	fun([LLActual, Expected]) ->
		[?_assertEqual(Expected, LLActual)]
	end
      }
    }.

api_insert_test_() ->
    {
      "When function `insert/3` is invoked on a given Linked List providing the Nth and data, then a new node is inserted to be the Nth node, on the Linked List, having the data provided",
      {
	setup,
	fun() -> LL = lns:new(), setup(LL, 3, fun lns:append/2), lns:insert(LL, 2, 'v4'), [LL] end,
	fun([LL]) ->
		[?_assertMatch([{Key1, v1}, {_Key2, v4}, {_Key3, v2}, {_Key4, v3}] when Key1 == 1, lns:to_list(LL))]
	end
      }
    }.

api_insert_on_empty_LL_test_() ->
    {
      "When function `insert/3` is invoked on an empty given Linked List providing the Nth and data, then a new node is inserted to be the first node, on the Linked List, having the data provided",
      {
	setup,
	fun() -> LL = lns:new(), lns:insert(LL, 2, 'v4'), [LL] end,
	fun([LL]) ->
		[?_assertMatch([{Key, v4}] when Key == 1, lns:to_list(LL))]
	end
      }
    }.

api_insert_on_1node_LL_test_() ->
    {
      "When function `insert/3` is invoked on a Linked List having only one node providing `Nth` value to be greater than 2 and data, then a new node is inserted to be the third and last node, on the Linked List, having the data provided",
      {
	setup,
	fun() -> LL = lns:new(), setup(LL, 1, fun lns:append/2), lns:insert(LL, 5, 'v4'), [LL] end,
	fun([LL]) ->
		[?_assertMatch([{Key1, v1}, {Key2, v4}] when Key1 == 1; Key2 == 576460752303423488, lns:to_list(LL))]
	end
      }
    }.

insert(LL, Nth) ->
    insert(LL, 5764, 1, Nth).

insert(_LL, N, N, _Nth) ->
    true;
insert(LL, N, I, Nth) ->
    lns:insert(LL, Nth, list_to_atom(lists:concat(['v', I]))),
    insert(LL, N, I + 1, Nth).

api_incapability_of_insert_on_very_close_values_of_existent_KeyPrev_and_KeyNext_nodes_LL_test_() ->
    {
      "When function `insert/3` is invoked on a Linked List having an arbitrary number of nodes, providing an arbitrary `Nth` value to fall within the current size of Linked List provided and data, and it happens that the desired `Nth` value comes in between two keys: `KeyPrev` and `KeyNext` with delta of `1`, then a new node won't be inserted and function `insert/3` must return `incapable`",
      {
	setup,
	fun() -> LL = lns:new(), setup(LL, 1, fun lns:append/2), insert(LL, 2), [LL] end,
	fun([LL]) ->
		[?_assertEqual(incapable, lns:insert(LL, 2, 'vx'))]
	end
      }
    }.

api_capability_of_insert_Nth_test_() ->
    {
      "Max capability to insert a `Nth` node in between; given %%% - increment_key = 576460752303423487).
%%%  and - decrement_key = -134217727); and `key_in_between`'s computation is with `bsr 1`",
      {
	setup,
	fun() -> fun() -> LL = lns:new(), lns:append(LL, 1), [lns:insert(LL, 2, 'vx') || _ <- lists:seq(1, 58)], LL end end,
	fun(F1) ->
		[?_assertEqual(true, lns:insert(F1(), 2, 'vx'))]
	end
      }
    }.

api_incapability_of_insert_more_than_58_nodes_in_between_test_() ->
    {
      "Incapability to insert a `Nth` node more than `58 nodes` in between; given %%% - increment_key = 576460752303423487).
%%%  and - decrement_key = -134217727); and `key_in_between`'s computation is with `bsr 1`. In addition, the number of total nodes from original `PrevKey` to `NextKey` is 60 inclusive. Any more
inserted nodes is neglected -- notice lists:seq(1, 90) in the setup fun's generator",
      {
	setup,
	fun() -> LL = lns:new(), lns:append(LL, 1), [lns:insert(LL, 2, 'vx') || _ <- lists:seq(1, 90)], [LL] end,
	fun([LL]) ->
		[?_assertEqual(incapable, lns:insert(LL, 2, 'vx')),
		 ?_assertEqual(60, length(lns:to_list(LL)))]
	end
      }
    }.

%% 46> F1 = fun() -> Lns = lns:new(), lns:append(Lns, 1), [lns:insert(Lns, 2, 'vx') || _ <- lists:seq(1, 58)], Lns end.
%% #Fun<erl_eval.20.99386804>
%% 47> lns:insert(F1(), 2, 'vx').
%% true

%% ok
%% 29> F1 = fun() -> Lns = lns:new(), lns:append(Lns, 1), [lns:insert(Lns, 2, 'vx') || _ <- lists:seq(1, 60)], Lns end.
%% #Fun<erl_eval.20.99386804>
%% 30> length(lns:to_list(F1())).
%% 60
%% 31> F1 = fun() -> Lns = lns:new(), lns:append(Lns, 1), [lns:insert(Lns, 2, 'vx') || _ <- lists:seq(1, 61)], Lns end.
%% ** exception error: no match of right hand side value #Fun<erl_eval.20.99386804>
%% 32> f().
%% ok
%% 33> F1 = fun() -> Lns = lns:new(), lns:append(Lns, 1), [lns:insert(Lns, 2, 'vx') || _ <- lists:seq(1, 61)], Lns end.
%% #Fun<erl_eval.20.99386804>
%% 60

%% [{1,vx},
%%  {2,vx},
%%  {4,vx},
%%  {8,vx},
%%  {16,vx},
%%  {32,vx},
%%  {64,vx},
%%  {128,vx},
%%  {256,vx},
%%  {512,vx},
%%  {1024,vx},
%%  {2048,vx},
%%  {4096,vx},
%%  {8192,vx},
%%  {16384,vx},
%%  {32768,vx},
%%  {65536,vx},
%%  {131072,vx},
%%  {262144,vx},
%%  {524288,vx},
%%  {1048576,vx},
%%  {2097152,vx},
%%  {4194304,vx},
%%  {8388608,vx},
%%  {16777216,vx},
%%  {33554432,vx},
%%  {67108864,vx},
%%  {134217728,...},
%%  {...}|...]
%% 43>

api_insert_0th_or_any_LessThanOne_test_() ->
    {
      "when function `insert/3` is invoked with `Nth` value being less than one, then no computation will be conducted and the function must return `neglected`", %% This is s tentative bug
      {
	setup,
	fun() -> fun() -> Lns = lns:new(), lns:append(Lns, v1), lns:insert(Lns, 0, 'vx') end end,
	fun(Actual) ->
		[?_assertEqual(neglected, Actual())]
	end
      }
    }.

api_insert_given_Nth_to_be_accidently_equals_the_head_key_test_() ->
    {
      "when function `insert/3` is invoked with `Nth` value being equals the head's key, then the new node will be inserted to be the second", %% This is s tentative bug
      {
	setup,
	fun() -> fun() -> Lns = lns:new(), lns:append(Lns, v1), lns:insert(Lns, 1, 'vx') end end,
	fun(Actual) ->
		[?_assertEqual(cannot_replace_the_head, Actual())]
	end
      }
    }.

%% 91> F1 = fun() -> Lns = lns:new(), lns:append(Lns, v1), lns:insert(Lns, 1, vx), Lns end.
%% #Fun<erl_eval.20.99386804>
%% 92> lns:to_list(F1()).
%% [{1,v1},{576460752303423488,vx}]
%% 93>
