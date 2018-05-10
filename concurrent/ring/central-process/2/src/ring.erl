-module(ring).
-export([start/2, stop/1, fetch_message/1,
	 init/1]).

start(N, Message) ->
    Nodes = start(N, 0, []),
    hd(Nodes) ! {new_message, self(), Message},
    {ok, Nodes}.

stop(Node) ->
    Node ! quit,
    {ok, noreply}.

fetch_message(NodePid) ->
    NodePid ! {fetch_message, self()},
    receive
	{reply, Message} ->
	    Message
    end.

start(0, 0, Nodes) ->
    First = lists:last(Nodes),
    set_parent(First, hd(Nodes)),
    Nodes;
start(N, I, []) ->
    start(N - 1, I, [spawn_node({client, self()})]);
start(N, I, Nodes = [H|_]) ->
    NextNode = spawn_node(H),
    start(N -1, I, [NextNode|Nodes]).

spawn_node({client, _Pid}) ->
    spawn(?MODULE, init, [undefined]);
spawn_node(ParentPid) ->
    spawn(?MODULE, init, [ParentPid]).

init(undefined) ->
    loop({self(), undefiend});
init(Pid) when is_pid(Pid) ->
    loop({Pid, undefined}).

loop({Parent, LastMessage} = State) ->
    receive
	{new_message, _From, NewMessage} ->
	    Parent ! {new_message, self(), NewMessage},
	    NewState = {Parent, NewMessage},
	    loop(NewState);
	{fetch_message, From} ->
	    From ! {reply, LastMessage},
	    loop(State);
	{set_parent, NewParent} ->
	    loop({NewParent, LastMessage});
	quit ->
	    case Parent of
		undefined ->
		    ok;
	    	Pid when is_pid(Pid) ->
	    	    Parent ! quit
	    end
    end.

set_parent(Node, Parent) ->
    Node ! {set_parent, Parent}.
