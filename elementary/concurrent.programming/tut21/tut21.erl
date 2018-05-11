-module(tut21).
-export([start/1, ping/2, pong/0]).

ping(N, Pong_pid) ->
    link(Pong_pid),
    ping1(N, Pong_pid).

ping1(0, _) ->
    exit(ping);
ping1(N, Pong_pid) ->
    Pong_pid ! {ping, self()},
    receive
	pong ->
	    io:format("Ping received pong~n", [])
    end,
    ping1(N - 1, Pong_pid).

pong() ->
    process_flag(trap_exit, true),
    pong1().

pong1() ->
    receive
	{ping, Ping_pid} ->
	    io:format("Pong received ping~n", []),
	    Ping_pid ! pong,
	    pong1();
	{'EXIT', From, Reason} ->
	    io:format("pong exiting, got ~p~n", [{'EXIT', From, Reason}])
    end.

start(Ping_node) ->
    PongPid = spawn(tut21, pong, []),
    spawn(Ping_node, tut21, ping, [3, PongPid]).
