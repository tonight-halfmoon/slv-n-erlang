-module(handler).
-export([handle/2]).
-include("config.hrl").
-include("interface_server.hrl").
-include("intercommunication.hrl").

handle(Free, Allocated) -> 
    receive  % Only Server can request to free/allocate resources 
	#free_resource{server=?server, from_pid=FromPid, resource=Resource} ->
	    case free(Free, Allocated, FromPid, Resource) of
		{ok, NewFree, NewAllocated} ->
		    ?server ! #handler_reply{message=freed},
		    handle(NewFree, NewAllocated);
		error ->
		    ?server ! #handler_reply{message=error},
		    handle(Free, Allocated)
	    end;
	#allocate_resource{server=?server, from_pid=FromPid} ->
	    case allocate(Free, Allocated, FromPid) of
		{{yes, Resource}, NewFree, NewAllocated} ->
		    ?server ! #handler_reply{message={yes, Resource}},
		    handle(NewFree, NewAllocated);
		{no, []} ->
		    ?server ! #handler_reply{message=no},
		    handle([], Allocated)
	    end;
	#server_request_data{server=?server} ->  % Only Server can request data structure
	    DS = #data_structure{free=Free, allocated=Allocated},
	    ?server ! #handler_reply_data{data=DS},
	    handle(Free, Allocated)
    end.

free(Free, Allocated, FromPid, Resource) ->
    case lists:member({Resource, FromPid}, Allocated) of
	true ->
	    NewFree = [Resource|Free],
	    NewAllocated = lists:delete({Resource, FromPid}, Allocated),
	    {ok, NewFree, NewAllocated};
	false ->
	    error
    end.

allocate([R|Free], Allocated, FromPid) ->
    io:format("FromPid: ~p; Resource: ~p~n", [FromPid, R]),
    NewFree = Free,
    NewAllocated = [{R, FromPid}|Allocated],
    {{yes, R}, NewFree, NewAllocated};
allocate([], _Allocated, _FromPid) ->
    {no, []}.
