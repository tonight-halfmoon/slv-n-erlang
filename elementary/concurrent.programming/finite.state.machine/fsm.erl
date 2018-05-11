-module(fsm).
-export([start/0, s1/0,s2/0,s3/0,s4/0]).

start() ->
    spawn(?MODULE, s1, []).

s1() ->
    receive
	msg_c ->
	    io:format("received msg_c~n", []),
	    s3();
	msg_a ->
	    s2();
	_ ->
	    stop
    end.

s2() ->
    receive
	msg_h ->
	    s4();
	msg_x ->
	    s3();
	_ ->
	    stop
    end.

s3() ->
    io:format("I've been called~n", []),
    receive
	msg_b ->
	    io:format("received msg_b~n",[]),
	    s1();
	msg_y ->
	    s2();
	_ ->
	    stop
    end.

s4() ->
    receive
	msg_i ->
	    s3();
	_ ->
	    stop
    end.
