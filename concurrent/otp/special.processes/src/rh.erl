-module(rh).
-import(dh_lib, [pairwith_hash/1, keymember/2, keydelete/2, values/1]).
-export([init_dh/2]).
-export([system_continue/3, system_terminate/4,
	 write_debug/3]).
-include("config.hrl").
-include("resa_server.hrl").
-include("telecommunication.hrl").
-include_lib("eunit/include/eunit.hrl").

init_dh(Parent, {Free, []}) ->
    Deb = sys:debug_options([statistics, trace]),
    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
			   ?MODULE, #rh_started{pid=self(), name=?handler, state={Free, []}}),
    proc_lib:init_ack(Parent, {ok, self()}),
    process_flag(trap_exit, true),
    handle_hashed({pairwith_hash(Free), []}, Parent, Deb2).

handle_hashed(State = {Free, Allocated}, Parent, Deb) ->
    
    receive 
	{system, From, Request} ->
	    sys:handle_system_msg(Request, From, Parent, ?MODULE, Deb, State);

	#free_resource{server=?server, from_pid=FromPid, resource=Tent_bin} ->
	    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
				    ?MODULE, {in, #free_resource{server=?server, from_pid=FromPid, resource=Tent_bin}, FromPid}),
	    ResFun = fun(X) -> case is_binary(X) of
				   true -> binary_to_term(X);
				   false -> Deb3 = sys:handle_debug(Deb2, fun ?MODULE:write_debug/3,
								   ?MODULE, {{"Unexpected input term", X}, FromPid}),
					    ?server ! #rh_refused{reason=unexpected_data},
					    sys:handle_debug(Deb3, fun ?MODULE:write_debug/3,
							     ?MODULE, {out, #rh_refused{reason=unexpected_data}, ?server})
			       end
		     end,
	    Ress = ResFun(Tent_bin),
	    case free(Free, Allocated, FromPid, Ress) of
		{ok, NewFree, NewAllocated} ->
		    Deb3 = sys:handle_debug(Deb2, fun ?MODULE:write_debug/3,
					  ?MODULE, {{"Resource successfully freed up", Ress}, FromPid}),
		    ?server ! #rh_reply{message={freed, Ress}},
		    Deb4 = sys:handle_debug(Deb3, fun ?MODULE:write_debug/3,
					    ?MODULE,  {#rh_reply{message={freed, Ress}}, ?server}),
		    handle_hashed({NewFree, NewAllocated}, Parent, Deb4);
		
		{error, Reason} ->
		    ?server ! #rh_reply{message={error, Reason}},
		    Deb3 = sys:handle_debug(Deb2, fun ?MODULE:write_debug/3,
					    ?MODULE, {out, #rh_reply{message={error, Reason}}, ?server}),
		    handle_hashed(State, Parent, Deb3)
	    end;

	#allocate_resource{server=?server, from_pid=FromPid} ->
	    case allocate(Free, Allocated, FromPid) of
		{{allocated, Resource}, NewFree, NewAllocated} ->
		    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
					    ?MODULE, {{"Resource successfully allocated"}, FromPid, Resource}),
		    ?server ! #rh_reply{message={allocated, Resource}},
		    Deb3 = sys:handle_debug(Deb2, fun ?MODULE:write_debug/3,
					    ?MODULE, {out, #rh_reply{message={allocated, Resource}}, ?server}),
		    handle_hashed({NewFree, NewAllocated}, Parent, Deb3);
		{no_free_resource, []} ->
		    ?server ! #rh_reply{message=no},
		    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
					   ?MODULE, {out, #rh_reply{message=no}, ?server}),
		    handle_hashed(State, Parent, Deb2)
	    end;

	#server_request_data{server=?server} ->
	    Deb2 = sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
				    ?MODULE, {in, #server_request_data{server=?server}, ?server}),
	    % 'handler' replies with only a list or resources names without showing the actual data structure
	    DS = #data_structure{free=values(Free), allocated=values(Allocated)},
	    ?server ! #rh_reply_data{data=DS},
	    Deb3 = sys:handle_debug(Deb2, fun ?MODULE:write_debug/3,
				   ?MODULE, {out, #rh_reply_data{data=DS}, ?server}),
	    handle_hashed(State, Parent, Deb3);

	{'EXIT', From, Reason} ->
	    sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
			     ?MODULE, #rh_stopped{event='EXIT', reason=Reason, from=From});

	{stop, Reason, From} ->
	    sys:handle_debug(Deb, fun ?MODULE:write_debug/3,
			     ?MODULE, #rh_stopped{event=stop, reason=Reason, from=From}),
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

write_debug(Dev, Event, Name) ->
    io:format(Dev, "~p: event = ~p~n", [Name, Event]).

system_continue(Parent, Deb, State) ->
    handle_hashed(State, Parent, Deb).

system_terminate(Reason, _Parent, _Deb, _State) ->
    exit(Reason).
