-module(ring).
-export([spawn_node/0, spawn_next/1, parent_of/1,
	 init/1,
	 send_message/3, fetch_message/1, send_quit_message/1,
	 set_child/2,
	 start/2]).

-define(NOTEST, true).
-include_lib("eunit/include/eunit.hrl").

-record(state, {parent, message, child}).

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
    To ! {mailbox, From, Message}.

set_child(Parent, Child) ->
    Parent ! {set_child, Child}.

start(N, Message) ->
    start(N, 0, Message, []).

start(0, 0, Message, Nodes) ->
    send_message(self(), _Last = hd(Nodes), Message),
    Nodes;
start(N, I, Message, []) ->
    start(N - 1, I, Message, [spawn_node({client, self()})]);
start(N, I, Message, Nodes = [H|_]) ->
    NextNode = spawn_node(H),
    start(N -1, I, Message, [NextNode|Nodes]).

spawn_node({client, _Pid}) ->
    spawn(?MODULE, init, [root]);
spawn_node(ParentPid) ->
    spawn(?MODULE, init, [ParentPid]).

init(root) ->
    loop(#state{parent = self()});
init(ParentPid) ->
    loop(#state{parent = ParentPid}).

loop(State = #state{parent = Parent, message = _LastMessage, child = _Child}) ->
    receive
	{set_child, Node} ->
	    NewState = State#state{child = Node},
	    loop(NewState);
	{your_parent, From} ->
	    From ! {reply, Parent},
	    loop(State);
	{mailbox, From, NewMessage} ->
	    NewState = State#state{message = NewMessage},
	    send_message(From, Parent, NewMessage),
	    loop(NewState);
	{fetch_message, From} ->
	    From ! {reply, State#state.message},
	    loop(State);
	quit ->
	    %% ?assert(is_process_alive(Child)),
	    %% case is_pid(Child) andalso is_process_alive(Child) of 
	    %% 	true ->
	    %% 	    Child ! quit;
	    %% 	false ->
	    %% 	    ok
	    %% end,
	    ?assert(is_process_alive(Parent)),
	    case Parent == self() of
	    	true ->
	    	    ok;
	    	false ->    
	    	    send_quit_message(Parent),
		    ok
	    end
    end.
