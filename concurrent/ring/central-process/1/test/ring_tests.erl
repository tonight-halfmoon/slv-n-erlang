-module(ring_tests).
-include_lib("eunit/include/eunit.hrl").
-import(ring, [start/2, stop/1, fetch_message/1]).

start_ring_N_nodes_and_send_a_message_around_and_terminate_all_nodes_gracefully_test() ->
    N = 3,
    M = initial_message,

    {ok, Nodes} = start(N, M),

    lists:foreach(fun(Next) ->
			  ?assertMatch({M, _SendNode}, fetch_message(Next))
		  end, Nodes),

    stop(lists:nth(3, Nodes)),

    receive after 50 ->
		    ok
	    end,

    lists:foreach(fun(Next) ->
			  ?assertNot(is_process_alive(Next))
		  end, Nodes).
