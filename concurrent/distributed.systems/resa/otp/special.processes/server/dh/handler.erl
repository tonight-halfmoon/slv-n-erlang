-module(handler).
-import(dh_lib, [pairwith_hash/1, keymember/2, keydelete/2, values/1]).
-export([init_dh/2]).
-export([system_continue/3, system_terminate/4,
	 write_debug/3]).
-include("../../config/config.hrl").
-include("../config_internal.hrl").
-include("../interface_server.hrl").
-include("../../config/telecommunication.hrl").
-include_lib("eunit/include/eunit.hrl").

init_dh(_, {Free, []}) when not is_list(Free) ->
    {error, expected_data_type_list};
init_dh(_, {[], []}) ->
    {error, expected_non_empty_Free_list};
init_dh(Parent, {Free, []}) ->
    Deb = sys:debug_options([statistics, trace]),
    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
			   ?MODULE, #dh_started{pid=self(), name=?handler, state={Free, []}}),
    proc_lib:init_ack(Parent, {ok, self()}),
    process_flag(trap_exit, true),
    handle_hashed({pairwith_hash(Free), []}, Parent, Deb2);
init_dh(_, {_Free, _Allocated}) ->
    {error, not_interested}.

handle_hashed(State = {Free, Allocated}, Parent, Deb) ->
    receive 

	{system, From, Request} ->
	    sys:handle_system_msg(Request, From, Parent, ?MODULE, Deb, State);

	#free_resource{server=?server, from_pid=FromPid, resource=Tent_bin} ->
	    ResFun = fun(X) -> case is_binary(X) of
				   true -> binary_to_term(X);
				   false -> io:format("Unexpected term ~p~n", [X]),
					    ?server ! #handler_refused{reason=unexpected_data} end end,
	    Ress = ResFun(Tent_bin),
	    case free(Free, Allocated, FromPid, Ress) of
		{ok, NewFree, NewAllocated} ->
		    io:format("FromPid ~p; Resource successfully freed up: ~p~n", [FromPid, Ress]),
		    ?server ! #handler_reply{message={freed, Ress}},
		    handle_hashed({NewFree, NewAllocated}, Parent, Deb);
		{error, Reason} ->
		    ?server ! #handler_reply{message={error, Reason}},
		    handle_hashed(State, Parent, Deb)
	    end;

	#allocate_resource{server=?server, from_pid=FromPid} ->
	    case allocate(Free, Allocated, FromPid) of
		{{allocated, Resource}, NewFree, NewAllocated} ->
		    io:format("FromPid ~p; Resource successfully allocated: ~p~n", [FromPid, Resource]),
		    ?server ! #handler_reply{message={allocated, Resource}},
		    handle_hashed({NewFree, NewAllocated}, Parent, Deb);
		{no_free_resource, []} ->
		    ?server ! #handler_reply{message=no},
		    handle_hashed(State, Parent, Deb)
	    end;

	#server_request_data{server=?server} -> 
	    % 'handler' replies with only a list or resources names without showing the actual data structure
	    DS = #data_structure{free=values(Free), allocated=values(Allocated)},
	    ?server ! #handler_reply_data{data=DS},
	    handle_hashed(State, Parent, Deb);

	{'EXIT', From, Reason} ->
	    sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
			     ?MODULE, #dh_stopped{event='EXIT', reason=Reason, from=From});

	{stop, Reason, From} ->
	    sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
			     ?MODULE, #dh_stopped{event=stop, reason=Reason, from=From}),
	    exit(Reason)
    end.

%%% Different clients can free up / allocate resources. For simolicity no constraints who allocate/freeup which.

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
      "Function 'init_dh/2' expects to be invoked with a non-empty Free list",
      ?_assertEqual({error, expected_non_empty_Free_list}, init_dh(self(), {[], []}))
    }.

handler_case_not_list_data_type_free_test_() ->
    {
      "function 'init_dh/2' expects to be invoked with data type list",
      ?_assertEqual({error,expected_data_type_list}, init_dh(self(), {'',[]}))
    }.

write_debug(Dev, Event, Name) ->
    io:format(Dev, "~p: event = ~p~n", [Name, Event]).

system_continue(Parent, Deb, State) ->
    handle_hashed(State, Parent, Deb).

system_terminate(Reason, _Parent, _Deb, _State) ->
    exit(Reason).
