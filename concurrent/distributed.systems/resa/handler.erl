-module(handler).
-export([handle/2]).
-include("config.hrl").
-include("interface_server.hrl").
-include("intercommunication.hrl").

handle(Free, []) ->
    process_flag(trap_exit, true),
    handle_hashed(pairwith_hash(Free), []);
handle(_Free, _Allocated) ->
    not_interested.

handle_hashed(Free, Allocated) ->
    receive  % Only Server can request to free/allocate resources 
	#free_resource{server=?server, from_pid=FromPid, resource=Resource} ->
	    case free(Free, Allocated, FromPid, Resource) of
		{ok, NewFree, NewAllocated} ->
		    ?server ! #handler_reply{message=freed},
		    handle_hashed(NewFree, NewAllocated);
		error ->
		    ?server ! #handler_reply{message=error},
		    handle_hashed(Free, Allocated)
	    end;
	#allocate_resource{server=?server, from_pid=FromPid} ->
	    case allocate(Free, Allocated, FromPid) of
		{{allocated, Resource}, NewFree, NewAllocated} ->
		    ?server ! #handler_reply{message={yes, Resource}},
		    handle_hashed(NewFree, NewAllocated);
		{no, []} ->
		    ?server ! #handler_reply{message=no},
		    handle_hashed([], Allocated)
	    end;
	#server_request_data{server=?server} ->  % Only Server can request data structure
	    DS = #data_structure{free=Free, allocated=Allocated},  %%% Much better to take out the values from Free/Allocated and send it to stats provider in order to keep internal data structure unknown
	    ?server ! #handler_reply_data{data=DS},
	    handle_hashed(Free, Allocated);
	{'EXIT', From, Reason} ->
	    io:format("Received 'EXIT' flag from ~p for reason ~p~n", [From, Reason]),
	    exit(Reason);
	_ ->
	    io:foramt("unknown!~n", []),
	    handle_hashed(Free, Allocated)
    end.

free(Free, Allocated, FromPid, Resource) ->
    io:format("Allocated: ~p~n", [Allocated]),
    io:format("Free: ~p~n", [Free]),
    case keymember(erlang:phash2(Resource), Allocated) of
	true ->
	    Phashed = pairwith_hash(Resource),
	    NewFree = [Phashed|Free],
	    io:format("New Free: ~p~n", [NewFree]),
	    NewAllocated = lists:delete({Phashed, FromPid}, Allocated),
	    io:format("New Allocated: ~p~n", [NewAllocated]),
	    {ok, NewFree, NewAllocated};
	false ->
	    io:format("Not found ~n", []),
	    error
    end.

allocate([R|Free], Allocated, FromPid) ->
    io:format("FromPid: ~p; Resource allocated: ~p~n", [FromPid, R]),
    {{allocated, R#res_ds.value}, Free, [{R, FromPid}|Allocated]};
allocate([], _Allocated, _FromPid) ->
    {no, []}.

pairwith_hash(H) when not is_list(H) ->
    #res_ds{hash=erlang:phash2(H), value=H};
pairwith_hash(L) ->
    pairwith_hash(L, []).

pairwith_hash([], Hashp) ->
    Hashp;
pairwith_hash([H|T], Hashp) ->
    pairwith_hash(T, [pairwith_hash(H)|Hashp]).

keymember(_Hash, []) ->
    false;
keymember(Hash, [{#res_ds{hash=Hash, value=_}, _}|_]) ->
    true;
keymember(Hash, [_H|T]) ->
    keymember(Hash, T).
