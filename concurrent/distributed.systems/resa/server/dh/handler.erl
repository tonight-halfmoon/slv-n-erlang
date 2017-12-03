-module(handler).
-import(dh_lib, [pairwith_hash/1, keymember/2, keydelete/2]).
-export([handle/2]).
-include("../../config/config.hrl").
-include("../interface_server.hrl").
-include("../../config/telecommunication.hrl").
-include_lib("eunit/include/eunit.hrl").

handle(Free, []) when not is_list(Free) ->
    {error, expected_data_type_list};
handle([], []) ->
    {error, expected_non_empty_Free_list};
handle(Free, []) ->
    process_flag(trap_exit, true),
    handle_hashed(pairwith_hash(Free), []);
handle(_Free, _Allocated) ->
    {error, not_interested}.

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
		    io:format("FromPid ~p; Resource successfully freed up: ~p~n", [FromPid, Ress]),
		    ?server ! #handler_reply{message={freed, Ress}},
		    handle_hashed(NewFree, NewAllocated);
		{error, Reason} ->
		    ?server ! #handler_reply{message={error, Reason}},
		    handle_hashed(Free, Allocated)
	    end;
	#allocate_resource{server=?server, from_pid=FromPid} ->
	    case allocate(Free, Allocated, FromPid) of
		{{allocated, Resource}, NewFree, NewAllocated} ->
		    io:format("FromPid ~p; Resource successfully allocated: ~p~n", [FromPid, Resource]),
		    ?server ! #handler_reply{message={allocated, Resource}},
		    handle_hashed(NewFree, NewAllocated);
		{no_free_resource, []} ->
		    ?server ! #handler_reply{message=no},
		    handle_hashed([], Allocated)
	    end;
	#server_request_data{server=?server} ->  %% Only Server can request data structure
	    DS = #data_structure{free=Free, allocated=Allocated},  %% it would be much better to take out the values from Free/Allocated and
	    ?server ! #handler_reply_data{data=DS},                %% send it to stats provider in order to keep internal data structure hidden
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

free(Free, Allocated, FromPid, Resource) ->
    case keymember(erlang:phash2(Resource), Allocated) of
	true ->
	    Resds = pairwith_hash(Resource),
	    {ok, [Resds|Free], keydelete(Resds#res_ds.hash, Allocated)};
	false ->
	    Reason = not_allocated,
	    io:format("FromPid ~p; Resource ~p not freed up; Reason: ~p~n", [FromPid, Resource, Reason]),
	    {error, Reason}
    end.

allocate([R|Free], Allocated, FromPid) ->
    {{allocated, R#res_ds.value}, Free, [{R, FromPid}|Allocated]};
allocate([], _Allocated, _FromPid) ->
    {no_free_resource, []}.

%%% Unit Tests

allocate_case1_test_() ->
    Res = 'ab.12.0',
    [R|Free] = [pairwith_hash(Res)],
    Pid = spawn(fun() -> [] end),
    {
      "When there is at least one free resource, then one resource must be allocted.", %  Allocate definition: Moving the resource from Free list to Allocated list.
      ?_assertEqual({{allocated, Res}, Free, [{R, Pid}]}, allocate([R|Free], [], Pid))
    }.

allocated_case2_test_() ->
    Pid = spawn(fun() -> [] end),
    {
      "When Free list is empty then allocate a resource is not possible.",
      ?_assertEqual({no_free_resource, []}, allocate([],[], Pid))
    }.

free_case1_test_() ->
    Res = 'ab.12.0',
    Resds = pairwith_hash(Res),
    Pid = spawn(fun() -> [] end),
    Allocated = [{Resds, Pid}],
    {
      "When a given resource is allocated, then the resource must be freed up. ", % Free up definition: Moving the resource from Allocated list to Free list.
      ?_assertEqual({ok, [Resds], []}, free(_Free=[], Allocated, Pid, Res))
    }.

free_case2_test_() ->
    Res = 'ab.12.0',
    Pid = spawn(fun() -> [] end),
    {
      "When a given resource is not allocated, then the resource cannot be freed up.",
      ?_assertEqual({error, not_allocated}, free([], [], Pid, Res))
    }.

handle_case_empty_free_list_test_() ->
    {
      "Function 'handle/2' expects to be invoked with a non-empty Free list",
      ?_assertEqual({error, expected_non_empty_Free_list}, handle([],[]))
    }.

handler_case_not_list_data_type_free_test_() ->
    {
      "function 'handle/2' expects to be invoked with data type list",
      ?_assertEqual({error,expected_data_type_list}, handle('',[]))
    }.
