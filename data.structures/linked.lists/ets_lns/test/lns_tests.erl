%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2018, 
%%% @doc
%%%
%%% @end
%%% On the 20th of February 2018
%%%-------------------------------------------------------------------

-module(lns_tests).

-export([run_suite/0]).

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
	fun() -> Lns = lns:new(), setup(Lns), lns:push(Lns, 'v3'), [Lns, {-1, 'v3'}] end,
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
		[?_assertEqual({-1, 'v3'}, lns:nth(0, Lns))]
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
		[?_assertEqual({0, 'v2'}, lns:nth(0, Lns))]
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
		[?_assertEqual({3, 'v3'}, lns:nth(2, Lns))]
	end
      }
    }.

api_to_list_test_() ->
    {
      "when function `to_list` is invoked on a Linked List, then it must return a list of nodes found in the input Linked List",
      {
	setup,
	fun() -> LL = lns:new(), setup(LL, 3), [LL, [{-1,'v3'}, {0,'v2'}, {1,'v1'}]] end,
	fun([LL, Expected]) ->
		[?_assertEqual(Expected, lns:to_list(LL))]
	end
      }
    }.

api_from_list_test_() ->
    {
      "When function `from_list` is invoked with a Erlang list as input, then it must return a Linked Lists of nodes, each node having the next index value from the Erlang List input",
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
      "When function `pop` is invoked on a given linked list, then it must extract the data from the head, delete the node, advance the head pointer to point at the next node in line",
      {
	setup,
	fun() -> LL = lns:new(), setup(LL, 3, fun lns:append/2), Expected = [{2,v2},{3,v3}],
		 {_Head , LLActual} = lns:pop(LL),
		 [LLActual, Expected] end,
	fun([LLActual, Expected]) ->
		[?_assertEqual(Expected, lns:to_list(LLActual))]
	end
      }
    }.
