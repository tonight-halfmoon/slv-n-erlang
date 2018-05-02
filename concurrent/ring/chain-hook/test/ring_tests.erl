-module(ring_tests).
-include_lib("eunit/include/eunit.hrl").
-import(ring, [spawn_node/0, spawn_next/1, parent_of/1,
	       send_message/3, fetch_message/1, send_quit_message/1,
	       set_child/2,
	       start/2]).

new_node_and_spawn_next_node_in_the_ring_test() ->
    NewNodePid = spawn_node(),
    NextNodePid = spawn_next(NewNodePid),

    NewNodePid = parent_of(NextNodePid),
    ?assert(is_process_alive(NewNodePid)),
    ?assert(is_process_alive(NextNodePid)),
    NewNodePid = parent_of(NewNodePid).

node_send_message_to_the_next_test() ->
    FirstNode = spawn_node(),
    NextNode = spawn_next(FirstNode),
    Message = 'how are you?',

    send_message(FirstNode, NextNode, Message),

    Reply = fetch_message(NextNode),

    ?assertEqual(Message, Reply).

when_node_receive_quit_message_then_then_node_and_the_parent_terminate_gracefully_test() ->
    ParentNode = spawn_node(),
    ChildNode = spawn_next(ParentNode),

    send_quit_message(ChildNode),

    receive after 50 -> ok end,
    ?assertNot(is_process_alive(ParentNode)),
    ?assertNot(is_process_alive(ChildNode)).

start_ring_test() ->
    N = 5,
    Message = "hello",
    Nodes = start(N, Message),
    Last = hd(Nodes),

    send_quit_message(Last),
    receive after 50 -> ok end,
    lists:foreach(fun(Next) -> ?assertNot(is_process_alive(Next)) end, Nodes).
