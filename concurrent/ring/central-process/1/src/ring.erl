-module(ring).
-export([start/2, stop/1, fetch_message/1,
	 loop/2]).

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

loop(Previous, Message) ->
    receive
	{fetch_message, From} ->
	    From ! {reply, self(),  {Message, Previous}},
	    loop(Previous, Message);
	{new_message, Node, NewMessage} ->
	    loop(Node, NewMessage);
	quit ->
	    case Previous of
		undefined ->
		    ok;
		Pid when is_pid(Pid) ->
		    Previous ! quit
	    end
    end.

spawn_node() ->
    spawn(?MODULE, loop, [undefined, []]).
