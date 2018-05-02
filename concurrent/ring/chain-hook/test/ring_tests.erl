-module(ring_tests).
-include_lib("eunit/include/eunit.hrl").
-import(ring, [spawn_node/0, spawn_next/1, parent_of/1,
	       send_message/3, fetch_message/1, send_quit_message/1,
	       start/2]).

new_node_and_spawn_next_node_in_the_ring_test() ->
    NewNodePid = spawn_node(),
    NextNodePid = spawn_next(NewNodePid),

    NewNodePid = parent_of(NextNodePid),
    ?assert(is_process_alive(NewNodePid)),
    ?assert(is_process_alive(NextNodePid)),
    NewNodePid = parent_of(NewNodePid),
    send_quit_message(NextNodePid).

node_send_message_to_the_next_test() ->
    FirstNode = spawn_node(),
    NextNode = spawn_next(FirstNode),
    Message = 'how are you?',

    send_message(FirstNode, NextNode, Message),

    Reply = fetch_message(NextNode),

    ?assertEqual(Message, Reply),
    send_quit_message(NextNode).

when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test() ->
    ParentNode = spawn_node(),
    ChildNode = spawn_next(ParentNode),

    send_quit_message(ChildNode),

    receive after 50 -> ok end,
    ?assertNot(is_process_alive(ParentNode)),
    ?assertNot(is_process_alive(ChildNode)).

start_ring_and_send_a_message_around_and_terminate_all_nodes_gracefully_test() ->
    N = 5,
    Message = "hello",
    Nodes = start(N, Message),

    lists:foreach(fun(Next) -> ?assertEqual(Message, fetch_message(Next)) end, Nodes),

    send_quit_message(lists:last(Nodes)),
    receive after 50 -> ok end,
    lists:foreach(fun(Next) -> ?assertNot(is_process_alive(Next)) end, Nodes).
