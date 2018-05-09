-module(ring_tests).
-include_lib("eunit/include/eunit.hrl").
-import(ring, [start/2, spawn_node/0, send_message/3, fetch_message/1]).

start_ring_n_nodes_test() ->
    N = 3,
    M = initial_message,

    Nodes = start(N, M),

    ?assertEqual(N, length(Nodes)).

spawn_node_test() ->
    NodePid = spawn_node(),

    ?assertMatch(Pid when is_pid(Pid), NodePid).

node_send_message_to_node_test() ->
    NodePid = spawn_node(),
    NodePid2 = spawn_node(),
    Message = hello,

    send_message(NodePid, NodePid2, Message),

    Reply = fetch_message(NodePid2),
    ?assertEqual({Message, NodePid}, Reply).

when_node_receive_quit_message_then_the_node_terminate_test() ->
    NodePid = spawn_node(),
    NodePid2 = spawn_node(),

    send_message(NodePid, NodePid2, quit),

    receive after 50 -> ok end,
    ?assertNot(is_process_alive(NodePid2)).

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
    N = 3,
    Nodes = start(N, Message),

    send_message(self(), hd(Nodes), quit),
    receive after 50 -> ok end,

    ?assertNot(is_process_alive(hd(Nodes))),
    ?assertNot(is_process_alive(lists:nth(2, Nodes))),
    ?assertNot(is_process_alive(lists:last(Nodes))).
