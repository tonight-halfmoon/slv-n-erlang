-module(rh).

-behaviour(gen_server).

-export([start_link/1]).
-export([freeup/1, alloc/1, rbrief/1]).

-export([init/1]).

-export([handle_cast/2, handle_call/3, handle_info/2, 
	 terminate/2, code_change/3]).

-include("rh.hrl").
-include("config.hrl").
-include("genrs_internal.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(dh_lib, [pairwith_hash/1, keymember/2, keydelete/2, values/1]).

-record(state, {free, allocated}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Free) ->
    gen_server:start_link({local, ?rh}, ?MODULE, {Free, []}, [{debug, [trace, statistics]}]).

freeup(R_protocol) ->
    ?server ! gen_server:call(?rh, R_protocol).

alloc(R_protocol) ->
    ?server ! gen_server:call(?rh, R_protocol).

rbrief(R_protocol) ->
    self() ! gen_server:call(?rh, R_protocol).

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
handle_call(#cask_dbrief{} = _Request, _From, #state{free=Free, allocated=Allocated} = State) ->
    DS = #data_structure{free=values(Free), allocated=values(Allocated)},
    Reply = #rh_dbrief{ds=DS},
    {reply, Reply, State};
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

%%%===================================================================
%%% Unit Tests
%%%===================================================================

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
      ?_assertEqual({'no free resource anymore', []}, allocate([],[], Pid))
    }.

free_case_not_binary_test_() ->
    Res = 'ab.12.0',
    Resds = pairwith_hash(Res),
    Pid = spawn(fun() -> [] end),
    Allocated = [{Resds, Pid}],
    {
      "When a given resource is not in binary data type, then Server must not handle the call.",
      ?_assertEqual({error, not_binary}, free(_Free=[], Allocated, Pid, Res))
    }.

free_case1_test_() ->
    Res = 'ab.12.0',
    Resds = pairwith_hash(Res),
    Pid = spawn(fun() -> [] end),
    Allocated = [{Resds, Pid}],
    {
      "When a given resource is allocated and client called to free it up, then Server must free up the resource",
      ?_assertEqual({ok, [Resds], []}, free(_Free=[], Allocated, Pid, term_to_binary(Res)))
    }.

free_case2_test_() ->
    Res = 'ab.12.0',
    Pid = spawn(fun() -> [] end),
    {
      "When a given resource is not allocated, then the resource cannot be freed up.",
      ?_assertEqual({error, 'resource has not been allocated'}, free([], [], Pid, term_to_binary(Res)))
    }.
