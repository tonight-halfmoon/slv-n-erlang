-module(ring).

-export([start/2, send_message/3, fetch_message/1]).

-include_lib("eunit/include/eunit.hrl").

-define(NOTEST, true).

-record(state, {message, from}).

start(N, M) ->
    start(N, 0, [], M).

start(0, 0, Nodes, M) ->
    Last = lists:last(Nodes),
    Head = hd(Nodes),
    send_message(Last, Head, M),
    Nodes;
start(N, I, [], M) ->
    Node = spawn_node(),
    start(N - 1, I, [Node], M);
start(N, I, Nodes = [H|_], M) ->
    Node = spawn_node(),
    send_message(Node, H, M),
    start(N - 1, I, [Node|Nodes], M).

node_loop(State = #state{message = Message, from = PrevNode}) ->
    receive
	{fetch_message, From} ->
	    From ! {reply, self(),  {Message, PrevNode}},
	    node_loop(State);
	{new_message, Node, NewMessage} ->
	    NewState = #state{message= NewMessage, from = Node},
	    node_loop(NewState);
	quit ->
	    Alive = is_pid(PrevNode) andalso is_process_alive(PrevNode),
	    if  Alive == true ->
		    send_message(self(), PrevNode, quit),
		    ok;
		Alive == false ->
		    ok
	    end
    end.

spawn_node() ->
    spawn(fun() -> node_loop(#state{}) end).

send_message(_SendNode, RecvNode, quit) ->
    RecvNode ! quit;
send_message(SendNode, RecvNode, Message) ->
    RecvNode ! {new_message, SendNode, Message},
    ok.

fetch_message(RecPid) ->
    RecPid ! {fetch_message, self()},
    receive
	{reply, RecPid, FetchedMessage} ->
	    FetchedMessage
    end.
