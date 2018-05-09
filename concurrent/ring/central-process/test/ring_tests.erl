-module(ring_tests).
-include_lib("eunit/include/eunit.hrl").
-import(ring, [start/2, spawn_node/0, send_message/3, fetch_message/1]).

start_ring_n_nodes_test() ->
    N = 3,
    M = initial_message,

    Nodes = start(N, M),

    ?assertEqual(N, length(Nodes)).

send_message_around_the_ring_test() ->
    Message = initial_message,
    N = 3,
    Nodes = start(N, Message),
    LastNode = lists:last(Nodes),
    FirstNode = hd(Nodes),
    SecondNode = lists:nth(2, Nodes),
    ReplyFromFirstNode = fetch_message(FirstNode),
    ReplyFromSecondNode = fetch_message(SecondNode),
    ReplyFromLastNode = fetch_message(LastNode),

    ?assertMatch({Message, SendNode} when SendNode == LastNode, ReplyFromFirstNode),
    ?assertMatch({Message, SendNode} when SendNode == FirstNode, ReplyFromSecondNode),
    ?assertMatch({Message, SendNode} when SendNode == SecondNode, ReplyFromLastNode).

when_ring_nodes_receive_a_quit_message_then_they_terminate_gracefully_test() ->
    Message = initial_message,
    N = 5,
    Nodes = start(N, Message),

    send_message(self(), lists:nth(3, Nodes), quit),

    receive after 50 ->
		    ok
	    end,

    lists:foreach(
      fun(Next) ->
	      ?assertNot(is_process_alive(Next))
      end, Nodes).
