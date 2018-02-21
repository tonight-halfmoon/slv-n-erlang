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
    Data = 'v1',
    Node = {1, Data},
    {
      "When function `push/2` is invoked on an empty Linked List, then the first node of the Linked List must be the node just got pushed",
      {
	setup,
	fun() -> Lns = lns:new(), lns:push(Lns, Data), Lns end,
	fun(Lns) ->
		[?_assertEqual(Node, lns:head(Lns))]
	end
      }
    }.

api_push_2_test_() ->
    Data = 'v3',
    Head = {-1, Data},
    {
      "When function `push/2` is invoked on a Linked List, then the last element pushed is the head",
      {
	setup,
	fun() -> Lns = lns:new(), setup(Lns), lns:push(Lns, 'v3'), Lns  end,
	fun(Lns) ->
		[?_assertEqual(Head, lns:head(Lns))]
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

api_nth_setup_with_append_test_() ->
    {
      "When a client asks for the nth node of a Linked List, then the Nth node must be returned - setup with append/2",
      {
	setup,
	fun() -> Lns = lns:new(), setup(Lns, 3, fun lns:append/2), Lns end,
	fun(Lns) ->
		[?_assertEqual({3, 'v3'}, lns:nth(3, Lns))]
	end
      }
    }.
