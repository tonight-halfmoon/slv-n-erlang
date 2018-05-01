-module(ring).

-export([start/2, spawn_node/0, send_message/3, fetch_message/1]).

-include_lib("eunit/include/eunit.hrl").

-define(NOTEST, true).

-record(state, {message, from}).

start(N, M) ->
    start(N, 0, [], M).

start(0, 0, Nodes, _M) ->
    Nodes;
start(N, I, [], M) ->
    Node = spawn_node(),
    start(N - 1, I, [Node], M); 
start(N, I, Nodes = [H|_], M) ->
    Node = spawn_node(),
    send_message(Node, H, M),
    start(N - 1, I, [Node|Nodes], M).

node_proc(State = #state{message = Message, from = SendNode}) ->
    receive
	{fetch_message, From} ->
	    From ! {reply, self(),  {Message, SendNode}},
	    node_proc(State);
	{mailbox, From, NewMessage} ->
	    NewState = #state{message= NewMessage, from = From},
	    node_proc(NewState);
	{quit, _From} ->
	    SendNode ! {quit, self()},
	    ok
    end.

spawn_node() ->
    spawn(fun() -> node_proc(#state{}) end).

send_message(SendNode, RecvNode, quit) ->
    RecvNode ! {quit, SendNode}; 
send_message(SendNode, RecvNode, Message) ->
    RecvNode ! {mailbox, SendNode, Message},
    ok.

fetch_message(RecPid) ->
    RecPid ! {fetch_message, self()},
    receive
	{reply, RecPid, FetchedMessage} ->
	    FetchedMessage
    end.
