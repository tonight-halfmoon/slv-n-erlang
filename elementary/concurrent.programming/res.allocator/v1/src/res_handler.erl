-module(res_handler).
-export([handle/2]).
-include("resalloc_config.hrl").
-include("resalloc_interface.hrl").

handle(Free, Allocated) -> 
    receive  % Only Server can request to free/allocate resources 
	%{?server, FromPid, {free, Resource}} ->
	#free_resource{server=?server, from_pid=FromPid, resource=Resource} ->
	    case free(Free, Allocated, FromPid, Resource) of
		{ok, NewFree, NewAllocated} ->
		    FromPid ! {?server, ok},
		    handle(NewFree, NewAllocated);
		error ->
		    FromPid ! {?server, error},
		    handle(Free, Allocated)
	    end;
	#allocate_resource{server=?server, from_pid=FromPid} ->
	%{?server, FromPid, alloc} ->
	    case allocate(Free, Allocated, FromPid) of
		{{yes, Resource}, NewFree, NewAllocated} ->
		    FromPid ! {?server, {yes, Resource}},
		    handle(NewFree, NewAllocated);
		{no, []} ->
		    FromPid ! {?server, no},
		    handle([], Allocated)
	    end;
	#server_request_data{server=?server, from_pid=FromPid} ->   
	    DS = #data_structure{free=Free, allocated=Allocated},
	    ?server ! #handler_reply_data{from_pid=FromPid, data=DS},
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
    NewFree = Free,
    NewAllocated = [{R, FromPid}|Allocated],
    {{yes, R}, NewFree, NewAllocated};
allocate([], _Allocated, _FromPid) ->
    {no, []}.

