-module(handler).
-export([handle/2]).
-include("config.hrl").
-include("interface_server.hrl").
-include("intercommunication.hrl").

handle(Free, []) when not is_list(Free) ->
    list_expected;
handle(Free, []) ->
    process_flag(trap_exit, true),
    handle_hashed(pairwith_hash(Free), []);
handle(_Free, _Allocated) ->
    not_interested.

handle_hashed(Free, Allocated) ->
    receive  % Only Server can request to free/allocate resources
	#free_resource{server=?server, from_pid=FromPid, resource=Tent_bin} ->
	    Ress = fun(X) -> case is_binary(X) of
				 true -> binary_to_term(X);
				 false -> io:format("Unexpected~p~n", [X]),
					  ?server ! #handler_refused{reason=unexpected_data} end end,
	    case free(Free, Allocated, FromPid, Ress(Tent_bin)) of
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
		{no_free_resource, []} ->
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

%%% Different clients can free up / allocate resources. For simolicity no constraints who allocate/freeup which.
%%% Future Work: Freeup/allocation provide more realistic solution.

free(Free, Allocated, _FromPid, Resource) ->
    case keymember(erlang:phash2(Resource), Allocated) of
	true ->
	    Phashed = pairwith_hash(Resource),
	    {ok, [Phashed|Free], keydelete(Phashed#res_ds.hash, Allocated)};
	false ->
	    io:format("Either resource has not been allocated or different process is freeing it up, which has been allocated by another process. ~n", []),
	    error
    end.

allocate([R|Free], Allocated, FromPid) ->
    io:format("FromPid: ~p; Resource allocated: ~p~n", [FromPid, R]),
    {{allocated, R#res_ds.value}, Free, [{R, FromPid}|Allocated]};
allocate([], _Allocated, _FromPid) ->
    {no_free_resource, []}.

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

keydelete(Hash, L) ->
    keydelete(Hash, L, []).

keydelete(_Hash, [], RM) ->
    RM;
keydelete(Hash, [{#res_ds{hash=Hash, value=_}, _}|T], RM) ->
    lists:append(RM, T);
keydelete(Hash, [H|T], RM) ->
    keydelete(Hash, T, [H|RM]).
