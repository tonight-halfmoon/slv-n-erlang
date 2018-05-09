-module(ring).
-export([start/2,
	 init/1,
	 send_message/3, fetch_message/1, send_quit_message/1]).

-define(NOTEST, true).
-include_lib("eunit/include/eunit.hrl").

start(N, Message) ->
    Nodes = start(N, 0, []),
    send_message(self(), _First = hd(Nodes), Message),
    Nodes.

start(0, 0, Nodes) ->
    First = lists:last(Nodes),
    set_parent(First, hd(Nodes)),
    Nodes;
start(N, I, []) ->
    start(N - 1, I, [spawn_node({client, self()})]);
start(N, I, Nodes = [H|_]) ->
    NextNode = spawn_node(H),
    start(N -1, I, [NextNode|Nodes]).

spawn_node() ->
    spawn_node({client, self()}).

spawn_next(ParentPid) ->
    spawn_node(ParentPid).

parent_of(NodePid) ->
    NodePid ! {your_parent, self()},
    receive
	{reply, Parent} ->
	    Parent
    end.

fetch_message(NodePid) ->
    NodePid ! {fetch_message, self()},
    receive
	{reply, Message} ->
	    Message
    end.

send_quit_message(To) ->
    To ! quit.

send_message(From, To, Message) ->
    To ! {new_message, From, Message}.

spawn_node({client, _Pid}) ->
    spawn(?MODULE, init, [root]);
spawn_node(ParentPid) ->
    spawn(?MODULE, init, [ParentPid]).

init(root) ->
    loop({self(), mailbox});
init(ParentPid) ->
    loop({ParentPid, mailbox}).

loop({Parent, Mailbox} = State) ->
    receive
	{set_parent, NewParent} ->
	    loop({NewParent, Mailbox});
	{your_parent, From} ->
	    From ! {reply, Parent},
	    loop(State);
	{new_message, _From, NewMessage} ->
	    send_message(self(), Parent, NewMessage),
	    NewState = {Parent, NewMessage},
	    loop(NewState);
	{fetch_message, From} ->
	    From ! {reply, Mailbox},
	    loop(State);
	quit ->
	    case Parent == self() andalso is_process_alive(Parent) of
	    	true ->
	    	    ok;
	    	false ->
	    	    send_quit_message(Parent),
		    ok
	    end
    end.

set_parent(Node, Parent) ->
    Node ! {set_parent, Parent}.
