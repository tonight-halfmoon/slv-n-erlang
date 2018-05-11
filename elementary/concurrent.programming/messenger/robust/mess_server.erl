-module(mess_server).
-export([start/0, server/0]).
-include("mess_interface.hrl").

server() ->
    process_flag(trap_exit, true),
    server([]).

%%% the user list has the format [{ClientPid1, Name1}, {ClientPid2, Name2},...]
server(User_list) ->
    io:format("User list = ~p~n", [User_list]),
    receive
	#logon{client_pid=From, username=Name} ->
	    New_user_list = server_logon(From, Name, User_list),
	    server(New_user_list);
	{'EXIt', From, _} ->
	    New_user_list = server_logoff(From, User_list),
	    server(New_user_list);
	#message{client_pid=From, to_name=To, message=Message} ->
	    server_transfer(From, To, Message, User_list),
	    server(User_list)
    end.

start() ->
    register(messenger, spawn(?MODULE, server, [])).

server_logon(From, Name, User_list) ->
    case lists:keymember(Name, 2, User_list) of
	true ->
	    From ! #abort_client{message=user_exists_at_other_node},
	    User_list;
	false ->
	    From ! #server_reply{message=logged_on},
	    link(From),
	    [{From, Name}|User_list]
    end.

server_logoff(From, User_list) ->
    lists:keydelete(From, 1, User_list).

server_transfer(From, To, Message, User_list) ->
    case lists:keysearch(From, 1, User_list) of
	false ->
	    From ! #abort_client{message=you_are_not_logged_on};
	{value, {_, Name}} ->
	    server_transfer(From, Name, To, Message, User_list)
    end.
server_transfer(From, Name, To, Message, User_list) ->
    case lists:keysearch(To, 2, User_list) of
	false ->
	    From ! #server_reply{message=receiver_not_found};
	{value, {ToPid, To}} ->
	    ToPid ! #message_from{from_name=Name, message=Message},
	    From ! #server_reply{message=sent}
    end.
