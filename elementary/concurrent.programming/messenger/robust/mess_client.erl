-module(mess_client).
-export([client/2]).
-include("mess_interface.hrl").
 
client(Server_node, Name) ->
    {messenger, Server_node} ! #logon{client_pid=self(), username=Name},
    await_result(),
    client(Server_node).

client(Server_node) ->
    receive
	logoff ->
	    exit(normal);
	#message_to{to_name=ToName, message=Message} ->
	   {messenger, Server_node} !
		#message{client_pid=self(), to_name=ToName, message=Message},
	    await_result();
	{message_from, From_name, Message} ->
	    io:format("Message from ~p: ~p~n", [From_name, Message])
    end,
    client(Server_node).

await_result() ->
    receive
	#abort_client{message=Why} ->
	    io:format("~p~n", [Why]),
	    exit(normal);
	#server_reply{message=What} ->
	    io:format("~p~n", [What])
    after 5000 ->
	    io:format("No response from server~n", []),
	    exit(timeout)
	end.
