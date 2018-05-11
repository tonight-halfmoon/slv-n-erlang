-module(tut20).
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
	    io:format("Ping received pong ~n", [])
    end,
    ping1(N -1, pong_Pid).

pong() ->
    receive
	{ping, Ping_pid} ->
	    io:format("Pong received ping~n", []),
	    Ping_pid ! pong,
	    pong()
    end.

start(Ping_node) ->
    PongPid = spawn(tut20, pong , []),
    spawn(Ping_node, tut20, ping, [3, PongPid]).
