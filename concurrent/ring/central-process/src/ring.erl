-module(ring).
-export([start/3, stop/1, fetch_message/1,
	 init/1]).

start(M, N, Message) ->
    {Last, Nodes} = setup_ring(N),
    {ok, noreply} = send_messages(M, Last, Message),
    {ok, Nodes}.

stop(Node) ->
    Node ! quit,
    {ok, noreply}.

fetch_message(Node) ->
    Node ! {fetch_message, self()},
    receive
	{reply, Message} ->
	    Message
    end.

setup_ring(N) ->
    Nodes = spawn_nodes(N, 0, []),
    Last = hd(Nodes),
    Reversed = [H|_] = lists:reverse(Nodes),
    H ! {set_next, Last},
    {Last, Reversed}.

spawn_nodes(0, 0, Nodes) ->
    Nodes;
spawn_nodes(N, I, []) ->
    Node = spawn(?MODULE, init, [undefined]),
    spawn_nodes(N - 1, I, [Node]);
spawn_nodes(N, I, Nodes = [H|_T]) ->
    Next = spawn(?MODULE, init, [H]),
    spawn_nodes(N - 1, I, [Next|Nodes]).

init(undefined) ->
    loop({undefined, []});
init(Pid) when is_pid(Pid) ->
    loop({Pid, []}).

loop({Next, Messages} = State) ->
    receive
	{send_message, Message, _from} ->
	    Next ! {new_message, {Message, self()}},
	    loop(State);
	{new_message, {Message, Node}} when Node == self() ->
	    NewState = {Next, [Message|Messages]},
	    loop(NewState);
	{new_message, {Message, _Node} = MessageTuple} ->
	    Next ! {new_message, MessageTuple},
	    NewState = {Next, [Message|Messages]},
	    loop(NewState);
	{fetch_message, From} ->
	    From ! {reply, Messages},
	    loop(State);
	{set_next, NewNext} ->
	    NewState = {NewNext, Messages},
	    loop(NewState);
	quit ->
	    case Next of
		undefined ->
		    ok;
	    	Pid when is_pid(Pid) ->
		    Next ! quit
	    end;
	_M ->
	    ok
    end.

send_messages(M, Node, Message) ->
    send_message(M, 0, Node, Message).

send_message(M, M, _Node, _Message) ->
    {ok, noreply};
send_message(M, I, Node, Message) ->
    Node ! {send_message, Message, self()},
    send_message(M, I + 1, Node, Message).
