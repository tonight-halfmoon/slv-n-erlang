-module(ring_tests).
-include_lib("eunit/include/eunit.hrl").
-import(ring, [start/2, spawn_node/0, send_message/3, fetch_message/1]).

start_ring_n_nodes_and_send_a_message_around_and_terminate_all_nodes_gracefully_test() ->
	ok.
