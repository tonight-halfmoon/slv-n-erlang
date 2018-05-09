-module(ring).
-export([start/3, stop/1,
	 fetch_message/1,
	 init/0]).

start(M, N, Message) ->
    Nodes = spawn_nodes(N, 0, []),
    Reordered = [H|_] = lists:reverse(Nodes),
    send_messages(M, H, Message),
    {ok, noreply, Reordered}.

stop(First) ->
    First ! {quit, self()},
    {ok, noreply}.

fetch_message(Node) ->
    Node ! {fetch_message, self()},
    receive
	M ->
	    M
    end.

spawn_nodes(N, N, Nodes) ->
    Nodes;
spawn_nodes(N, I, []) ->
    Node = start_node(),
    spawn_nodes(N, I + 1, [Node]);
spawn_nodes(N, I, [H|_T] = Nodes) ->
    H ! {spawn_next, self()},
    Next = receive
	       Pid ->
		   Pid
	   end,
    spawn_nodes(N, I + 1, [Next|Nodes]).

start_node() ->
    spawn(?MODULE, init, []).

init() ->
    loop(undefined, []).

loop(Next, Msgs) ->
    receive
	{spawn_next, From} ->
	    Pid = spawn(?MODULE, init, []),
	    From ! Pid,
	    loop(Pid, Msgs);
	{quit, _From} ->
	    case Next of
		undefined ->
		    ok;
		Pid when is_pid(Pid) ->
		    Next ! {quit, self},
		    ok
	    end;
	{fetch_message, From} ->
	    From ! Msgs,
	    loop(Next, Msgs);
	{new_message, NewMessage, _From} ->
	    case Next of
		undefined ->
		    loop(Next, [NewMessage|Msgs]);
		Pid when is_pid(Pid) ->
		    Next ! {new_message, NewMessage, self()},
		    loop(Next, [NewMessage|Msgs])
	    end;
	_M ->
	    ok
    end.

send_messages(M, Node, Message) ->
    send_message(M, 0, Node, Message).

send_message(M, M, _Node, _Message) ->
    ok;
send_message(M, I, Node, Message) ->
    Node ! {new_message, Message, self()},
    send_message(M, I + 1, Node, Message).
