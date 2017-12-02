-module(handler).
-export([handle/2]).
-include("../config/config.hrl").
-include("interface_server.hrl").
-include("../config/telecommunication.hrl").
-include_lib("eunit/include/eunit.hrl").

handle(Free, []) when not is_list(Free) ->
    list_expected;
handle(Free, []) ->
    process_flag(trap_exit, true),
    handle_hashed(pairwith_hash(Free), []);
handle(_Free, _Allocated) ->
    not_interested.

handle_hashed(Free, Allocated) ->
    receive  %% Only Server can request to free/allocate resources
	#free_resource{server=?server, from_pid=FromPid, resource=Tent_bin} ->
	    ResFun = fun(X) -> case is_binary(X) of
				 true -> binary_to_term(X);
				 false -> io:format("Unexpected~p~n", [X]),
					  ?server ! #handler_refused{reason=unexpected_data} end end,
	    Ress = ResFun(Tent_bin),
	    case free(Free, Allocated, FromPid, Ress) of
		{ok, NewFree, NewAllocated} ->
		    io:format("FromPid: ~p; Resource successfully freed up: ~p~n", [FromPid, Ress]),
		    ?server ! #handler_reply{message={freed, Ress}},
		    handle_hashed(NewFree, NewAllocated);
		{error, Reason} ->
		    ?server ! #handler_reply{message={error, Reason}},
		    handle_hashed(Free, Allocated)
	    end;
	#allocate_resource{server=?server, from_pid=FromPid} ->
	    case allocate(Free, Allocated, FromPid) of
		{{allocated, Resource}, NewFree, NewAllocated} ->
		    io:format("FromPid: ~p; Resource successfully allocated: ~p~n", [FromPid, Resource]),
		    ?server ! #handler_reply{message={allocated, Resource}},
		    handle_hashed(NewFree, NewAllocated);
		{no_free_resource, []} ->
		    ?server ! #handler_reply{message=no},
		    handle_hashed([], Allocated)
	    end;
	#server_request_data{server=?server} ->  %% Only Server can request data structure
	    DS = #data_structure{free=Free, allocated=Allocated},  %% Much better to take out the values from Free/Allocated and send it to stats provider in order to keep internal data structure unknown
	    ?server ! #handler_reply_data{data=DS},
	    handle_hashed(Free, Allocated);
	{'EXIT', From, Reason} ->
	    io:format("Received 'EXIT' flag from ~p for reason ~p~n", [From, Reason]),
	    exit(Reason);
	Unknown ->
	    io:format("Received unconsidered message ~p~n", [Unknown]),
	    handle_hashed(Free, Allocated)
    end.

%%% Different clients can free up / allocate resources. For simolicity no constraints who allocate/freeup which.
%%% Future Work: Freeup/allocation provide more realistic solution.

free(Free, Allocated, _FromPid, Resource) ->
    case keymember(erlang:phash2(Resource), Allocated) of
	true ->
	    Resds = pairwith_hash(Resource),
	    {ok, [Resds|Free], keydelete(Resds#res_ds.hash, Allocated)};
	false ->
	    io:format("Resource not allocated: ~p~n", [Resource]),
	    {error, not_allocated}
    end.

allocate([R|Free], Allocated, FromPid) ->
    {{allocated, R#res_ds.value}, Free, [{R, FromPid}|Allocated]};
allocate([], _Allocated, _FromPid) ->
    {no_free_resource, []}.

pairwith_hash(R) when not is_list(R) ->
    #res_ds{hash=erlang:phash2(R), value=R};
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

%%% Unit Tests

allocate_test_() ->
    Res = 'ab.12.0',
    [R|Free] = [pairwith_hash(Res)],
    Pid = spawn(fun() -> [] end),
    {
      "When there is at least one free resource, then one resource must be allocted. Allocate: move the resource from Free list to Allocated list.",
     ?_assertEqual({{allocated, Res}, Free, [{R, Pid}]}, allocate([R|Free], [], Pid))
    }.

free_test_() ->
    Res = 'ab.12.0',
    Resds = pairwith_hash(Res),
    Pid = spawn(fun() -> [] end),
    Allocated = [{Resds, Pid}],
    {
      "When there is at least one allocated resource, then one resource must be freed up. Free up: Move it from Allocated list to Free list. Must not exist any more in Allocated list.",
      ?_assertEqual({ok, [Resds], []}, free(_Free=[], Allocated, Pid, Res))
    }.
