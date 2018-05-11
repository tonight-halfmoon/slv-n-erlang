-module(messenger).
-export([start_server/0, server/0, logon/1, logoff/0, message/2, client/2]).

%%% Change the functions below to return the name of the node where the
%%% messenger server runs
server_node() ->
    messenger@localhost.

%%% This is the server process for the "messenger"
%%% the user list has the format [{ClientPid1, Name1}, {ClientPid2, Name2}, ...]
server() ->
    process_flag(trap_exit, true),
    server([]).

server(Userlist) ->
    receive
	{From, logon, Name} ->
	    NewUserlist = server_logon(From, Name, Userlist),
	    server(NewUserlist);
	{'EXIT', From, _} ->
	    NewUserlist = server_logoff(From, Userlist),
	    server(NewUserlist);
	{From, message_to, To, Message} ->
	    server_transfer(From, To, Message, Userlist),
	    server(Userlist)
	end.

%%% Start the server
start_server() ->
    register(messenger, spawn(messenger, server, [])).

%%% Server adds a new user to the user list
server_logon(From, Name, Userlist) ->
    %% check if logged on on anywhere else
    case lists:keymember(Name, 2, Userlist) of
	true ->
	    From ! {messenger, stop, user_exists_at_other_node}, % reject logon
	    Userlist;
	false ->
	    From ! {messenger, logged_on},
	    link(From),
	    [{From, Name} | Userlist] % add user to the list
	end.

%%% Server deletes a user from the list
server_logoff(From, Userlist) ->
    lists:keydelete(From, 1, Userlist).


%%% Server transfers a message between users
server_transfer(From, To, Message, Userlist) ->
    %% check that the user is logged on and who he is 
    case lists:keysearch(From, 1, Userlist) of
	false ->
	    From ! {messenger, stop, you_are_not_logged_on};
	{value, {_, Name}} ->
	    server_transfer(From, Name, To, Message, Userlist)
	end.

%%% If the user exists, send the message
server_transfer(From, Name, To, Message, Userlist) ->
    case lists:keysearch(To, 2, Userlist) of
	false ->
	    From ! {messenger, receiver_not_found};
	{value, {ToPid, To}} ->
	    ToPid ! {message_from, Name, Message},
	    From ! {messenger, sent}
    end.

%%% User API
%%% User Commands
logon(Name) ->
    case whereis(mess_client) of
	undefined ->
	    register(mess_client, spawn(messenger, client, [server_node(), Name]));
	_ ->
	    already_logged_on
    end.

logoff() ->
    mess_client ! logoff.

message(ToName, Message) ->
    case whereis(mess_client) of
	undefined ->
	    not_logged_on;
	_ ->
	    mess_client ! {message_to, ToName, Message},
	    ok
    end.

%%% The client process which runs on each sever node
client(Server_node, Name) ->
    {messenger, Server_node} ! {self(), logon, Name},
    await_result(),
    client(Server_node).

client(Server_node) ->
    receive
	logoff ->
	    exit(normal);
	{message_to, ToName, Message} ->
	    {messenger, Server_node} ! {self(), message_to, ToName, Message},
	    await_result();
	{message_from, FromName, Message} ->
	    io:format("Message from ~p: ~p~n", [FromName, Message])
    end,
    client(Server_node).

await_result() ->
    receive
	{messenger, stop, Why} -> % Stop the client
	    io:format("~p~n", [Why]),
	    exit(normal);
	{messenger, What} -> % Normal response
	    io:format("~p~n", [What])
    after 5000 ->
	    io:format("No response from server~n", []),
	    exit(timeout)
	end.
