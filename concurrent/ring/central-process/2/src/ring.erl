-module(ring).
-export([start/3, stop/1, fetch_message/1,
	 init/1]).

start(M, N, Message) ->
    Nodes = spawn_nodes(N, 0, []),
    Reordered = [H|_] = lists:reverse(Nodes),
    Last = hd(Nodes),
    set_next(H, Last),
    send_messages(M, Last, Message),
    {ok, Reordered}.

stop(Node) ->
    Node ! quit,
    {ok, noreply}.

fetch_message(NodePid) ->
    NodePid ! {fetch_message, self()},
    receive
	{reply, Message} ->
	    Message
    end.

spawn_nodes(0, 0, Nodes) ->
    Nodes;
spawn_nodes(N, I, []) ->
    spawn_nodes(N - 1, I, [spawn_node(undefined)]);
spawn_nodes(N, I, Nodes = [H|_]) ->
    Next = spawn_node(H),
    spawn_nodes(N -1, I, [Next|Nodes]).

spawn_node(Args) ->
    spawn(?MODULE, init, [Args]).

init(undefined) ->
    loop({undefined, []});
init(Pid) when is_pid(Pid) ->
    loop({Pid, []}).

loop({Next, Messages} = State) ->
    receive
	{send_message, Message} ->
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
	    end
    end.

set_next(Node, Next) ->
    Node ! {set_next, Next}.

send_messages(M, Node, Message) ->
    send_message(M, 0, Node, Message).

send_message(M, M, _Node, _Message) ->
    ok;
send_message(M, I, Node, Message) ->
    Node ! {send_message, Message},
    send_message(M, I + 1, Node, Message).
