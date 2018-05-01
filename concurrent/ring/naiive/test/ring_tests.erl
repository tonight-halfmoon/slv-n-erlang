-module(ring_tests).
-include_lib("eunit/include/eunit.hrl").
-import(ring, [start/2, spawn_node/0, send_message/3, fetch_message/1]).

start_n_nodes_test() ->
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

node_send_quit_message_to_recv_node_then_recv_node_die_test() ->
    NodePid = spawn_node(),
    NodePid2 = spawn_node(),

    send_message(NodePid, NodePid2, quit),

    receive after 50 -> ok end,
    ?assertNot(is_process_alive(NodePid2)).

fetched_message_in_next_node_is_sent_from_the_adjacent_fromer_node_test() ->
    Message = initial_message,
    N = 3,
    Nodes = start(N, Message),
    LastNode = hd(lists:reverse(tl(Nodes))),
    NearestLastNode = lists:nth(N -1, Nodes),
    FirstNode = hd(Nodes),
    SecondNode = hd(lists:delete(FirstNode, Nodes)),

    Reply = fetch_message(LastNode),

    ?assertMatch({_Message, SendNode} when SendNode == NearestLastNode, Reply),

    ReplySecond = fetch_message(SecondNode),

    ?assertMatch({_Message, SendNode} when SendNode == FirstNode, ReplySecond).

%when_quit_message_sent_by_first_node_it_must_be_propagated_to_the_following_nodes_until_the_ring_stop_test() ->
%    Message = initial_message,
%    N = 3,
%    Nodes = start(N, Message),
%    FirstNode = hd(Nodes),
%    LastNode =  hd(lists:reverse(tl(Nodes)))
%	.
