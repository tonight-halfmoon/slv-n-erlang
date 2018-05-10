-module(ring).
-export([start/3, stop/1,
	 fetch_message/1,
	 init/0]).

start(M, N, Message) ->
    {Head, Nodes} = setup_ring(N),
    send_messages(M, Head, Message),
    {ok, Nodes}.

stop(First) ->
    First ! {quit, self()},
    {ok, noreply}.

fetch_message(Node) ->
    Node ! {fetch_message, self()},
    receive
	M ->
	    M
    end.

setup_ring(N) ->
    Nodes = spawn_nodes(N, 0, []),
    Reversed = [H|_] = lists:reverse(Nodes),
    hd(Nodes) ! {set_next, H},
    {H, Reversed}.

spawn_nodes(N, N, Nodes) ->
    Nodes;
spawn_nodes(N, I, []) ->
    Node = spawn(?MODULE, init, []),
    spawn_nodes(N, I + 1, [Node]);
spawn_nodes(N, I, [H|_T] = Nodes) ->
    H ! {spawn_next, self()},
    Next = receive
	       Pid ->
		   Pid
	   end,
    spawn_nodes(N, I + 1, [Next|Nodes]).

init() ->
    loop(undefined, []).

loop(Next, Msgs = State) ->
    receive
	{spawn_next, From} ->
	    Pid = spawn(?MODULE, init, []),
	    From ! Pid,
	    loop(Pid, Msgs);
	{set_next, Node} ->
	    loop(Node, State);
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
	{send_message, Message, _from} ->
	    Next ! {new_message, {Message, self()}},
	    loop(Next, State);
	{new_message, {Message, Node}} when Node == self() ->
	    NewState = [Message|Msgs],
	    loop(Next, NewState);
	{new_message, {Message, _Node} = MessageTuple} ->
	    Next ! {new_message, MessageTuple},
	    NewState = [Message|Msgs],
	    loop(Next, NewState);
	_M ->
	    ok
    end.

send_messages(M, Node, Message) ->
    send_message(M, 0, Node, Message).

send_message(M, M, _Node, _Message) ->
    ok;
send_message(M, I, Node, Message) ->
    Node ! {send_message, Message, self()},
    send_message(M, I + 1, Node, Message).
