-module(ring_tests).
-include_lib("eunit/include/eunit.hrl").
-import(ring, [start/3, stop/1, fetch_message/1]).

start_ring_N_nodes_and_send_M_message_around_and_terminate_all_nodes_gracefully_test() ->
    Message = initial_message,
    {ok, noreply, Nodes} = start(3, 3, Message),

    receive after 1 ->
		    ok
	    end,

    ExpectedMessages = [Message || _X <- lists:seq(1,3)],

    lists:foreach(fun(Next) ->
			  ?assertEqual(ExpectedMessages, fetch_message(Next)) end, Nodes),

    {ok, noreply} = stop(hd(Nodes)),

    receive after 1 ->
		    ok
	    end,

    lists:foreach(fun(Next) ->
			  ?assertNot(is_process_alive(Next)) end, Nodes).
