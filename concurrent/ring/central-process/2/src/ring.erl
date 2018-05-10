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
    NextNode = spawn_node(H),
    spawn_nodes(N -1, I, [NextNode|Nodes]).

spawn_node(Args) ->
    spawn(?MODULE, init, [Args]).

init(undefined) ->
    loop({{head, undefined}, []});
init(Pid) when is_pid(Pid) ->
    loop({{link, Pid}, []}).

loop({{Type, Next} = Node, Messages} = State) ->
    receive
	{new_message, _From, NewMessage} ->
	    case Node of
		{head, Next} ->
		    NewState = {{head, Next}, [NewMessage|Messages]},
		    loop(NewState);
		{link, Next} ->
		    NewState = {{Type, Next}, [NewMessage|Messages]},
		    Next ! {new_message, self(), NewMessage},
		    loop(NewState)
	    end;
	{fetch_message, From} ->
	    From ! {reply, Messages},
	    loop(State);
	{set_next, NewNext} ->
	    NewState = {{Type, NewNext}, Messages},
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
    Node ! {new_message, self(), Message},
    send_message(M, I + 1, Node, Message).
