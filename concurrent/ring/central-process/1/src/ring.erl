-module(ring).
-export([start/2, stop/1, fetch_message/1,
	 loop/1]).

-record(state, {message, from = undefined}).

start(N, M) ->
    Nodes = start(N, 0, [], M),
    {ok, Nodes}.

stop(Node) ->
    Node ! quit,
    {ok, noreply}.

fetch_message(RecPid) ->
    RecPid ! {fetch_message, self()},
    receive
	{reply, RecPid, FetchedMessage} ->
	    FetchedMessage
    end.

start(0, 0, Nodes, M) ->
    Last = lists:last(Nodes),
    Head = hd(Nodes),
    Head ! {new_message, Last, M},
    Nodes;
start(N, I, [], M) ->
    Node = spawn_node(),
    start(N - 1, I, [Node], M);
start(N, I, Nodes = [H|_], M) ->
    Node = spawn_node(),
    H ! {new_message, Node, M},
    start(N - 1, I, [Node|Nodes], M).

loop(State = #state{message = Message, from = PrevNode}) ->
    receive
	{fetch_message, From} ->
	    From ! {reply, self(),  {Message, PrevNode}},
	    loop(State);
	{new_message, Node, NewMessage} ->
	    NewState = #state{message= NewMessage, from = Node},
	    loop(NewState);
	quit ->
	    case PrevNode of
		undefined ->
		    ok;
		Pid when is_pid(Pid) ->
		    PrevNode ! quit
	    end
    end.

spawn_node() ->
    spawn(?MODULE, loop, [#state{}]).
