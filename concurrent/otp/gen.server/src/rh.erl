-module(rh).

-behaviour(gen_server).

-export([start_link/1]).
-export([freeup/1, alloc/1]).

-export([init/1]).

-export([handle_cast/2, handle_call/3, handle_info/2, 
	 terminate/2, code_change/3]).

-include("rh.hrl").
-include("config.hrl").
-include("genrs.hrl").
-include("telecommunication.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(dh_lib, [pairwith_hash/1, keymember/2, keydelete/2, values/1]).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Free) ->
    gen_server:start_link({local, ?rh}, ?MODULE, {Free, []}, [{debug, [trace, statistics]}]).

freeup(R_protocol) ->
    ?server ! gen_server:call(?rh, R_protocol).

alloc(R_protocol) ->
    ?server ! gen_server:call(?rh, R_protocol).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({Free, []}) ->
    process_flag(trap_exit, true),
    {ok, #state{free=dh_lib:pairwith_hash(Free), allocated=[]}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(#cask2free{resource=Res} = _Request, From, #state{free=Free, allocated=Allocated} = State) ->
    case free(Free, Allocated, From, Res) of
	{ok, NewFree, NewAllocated} ->
	    New_state = {NewFree, NewAllocated},
	    Reply = #rh_ok{more={'free up success', Res}, new_state=#state{free=NewFree, allocated=NewAllocated}},
	    {reply, Reply, New_state};
	{error, Reason} ->
	    Reply = #rh_error{reason=Reason},
	    {reply, Reply, State}
    end;
handle_call(#cask2alloc{} = _Request, _From, #state{free=Free, allocated=Allocated} = State) ->
    case allocate(Free, Allocated, self()) of
	{{allocated, _Res}, New_free, [{_R, _FromPid}|_AlcT] = New_allocated} ->
	    New_state = #state{free=New_free, allocated=New_allocated},
	    Reply = #rh_ok{more={'allocate success'}, new_state=New_state},
	    {reply, Reply, New_state};
	{'no free resource anymore' = Reason, []} ->
	    Reply = #rh_error{reason=Reason},
	    {reply, Reply, State}
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_info({'EXIT', _From, _Reason}, State) ->
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, _State) ->
    io:format("~p Shutdown because of ~p~n", [?rh, Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_hashed(State = {Free, Allocated}) ->    
    receive
	#free_resource{server=?server, from_pid=FromPid, resource=Tent_bin} ->
	    io:format("~p: ~p~n", [?MODULE, 
				   {in, #free_resource{server=?server, from_pid=FromPid, resource=Tent_bin}, FromPid}]),
	    ResFun = fun(X) -> case is_binary(X) of
				   true -> binary_to_term(X);
				   false ->
				       io:format("~p: ~p~n", [?MODULE, {{"Unexpected input term", X}, FromPid}]),
				       ?server ! #rh_refused{reason=unexpected_data},
				       io:format("~p: ~p~n", [?MODULE, {out, #rh_refused{reason=unexpected_data}, ?server}])
			       end
		     end,
	    Ress = ResFun(Tent_bin),
	    case free(Free, Allocated, FromPid, Ress) of
		{ok, NewFree, NewAllocated} ->
		    io:format("~p: ~p~n", [?MODULE, {{"Resource successfully freed up", Ress}, FromPid}]),
		   % ?server ! #rh_reply{message={freed, Ress}},
		    %io:format("~p: ~p~n", [?MODULE,  {#rh_reply{message={freed, Ress}}, ?server}]),
		    handle_hashed({NewFree, NewAllocated});
		
		{error, _Reason} ->
		    %?server ! #rh_reply{message={error, Reason}},
		   % io:format("~p: ~p~n", [?MODULE, {out, #rh_reply{message={error, Reason}}, ?server}]),
		    handle_hashed(State)
	    end;

	#allocate_resource{server=?server, from_pid=FromPid} ->
	    case allocate(Free, Allocated, FromPid) of
		{{allocated, Resource}, NewFree, NewAllocated} ->
		    io:format("~p: ~p~n", [?MODULE, {{"Resource successfully allocated"}, FromPid, Resource}]),
		   % ?server ! #rh_reply{message={allocated, Resource}},
		    %io:format("~p: ~p~n", [?MODULE, {out, #rh_reply{message={allocated, Resource}}, ?server}]),
		    handle_hashed({NewFree, NewAllocated});
		{no_free_resource, []} ->
		   % ?server ! #rh_reply{message=no},
		    %io:format("~p: ~p~n", [?MODULE, {out, #rh_reply{message=no}, ?server}]),
		    handle_hashed(State)
	    end;

	#server_request_data{server=?server} ->
	    io:format("~p: ~p~n", [?MODULE, {in, #server_request_data{server=?server}, ?server}]),
	    % 'handler' replies with only a list or resources names without showing the actual data structure
	    DS = #data_structure{free=values(Free), allocated=values(Allocated)},
	    %?server ! #rh_reply_data{data=DS},
	    io:format("~p: ~p~n", [?MODULE, {out, #rh_reply_data{data=DS}, ?server}]),
	    handle_hashed(State);

	{'EXIT', From, Reason} ->
	    io:format("~p: ~p~n", [?MODULE, #rh_stopped{event='EXIT', reason=Reason, from=From}]);

	{stop, Reason, From} ->
	    io:format("~p: ~p~n", [?MODULE, #rh_stopped{event=stop, reason=Reason, from=From}]),
	    exit(Reason)
    end.

%%% Different clients can free up / allocate resources. For simolicity no constraints who allocate/freeup which.

free(_, _, _FromPid, Not_binary) when not is_binary(Not_binary) ->
    Reason = not_binary,
    {error, Reason};
free(Free, Allocated, _FromPid, Binary) ->
    Resource = binary_to_term(Binary),
    case dh_lib:keymember(erlang:phash2(Resource), Allocated) of
	true ->
	    Resds = pairwith_hash(Resource),
	    {ok, [Resds|Free], keydelete(Resds#res_ds.hash, Allocated)};
	false ->
	    Reason = 'resource has not been allocated',
	    {error, Reason}
    end.

allocate([R|Free], Allocated, FromPid) ->
    {{allocated, R#res_ds.value}, Free, [{R, FromPid}|Allocated]};
allocate([], _Allocated, _FromPid) ->
    {'no free resource anymore', []}.

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
