%%%-------------------------------------------------------------------
%%% @author Taghrid Elghafari
%%% @copyright (C) 2018, Taghrid Elghafari
%%% @doc
%%%
%%% @end
%%% Created :  9 Aug 2018 by Taghrid Elghafari
%%%-------------------------------------------------------------------
-module(minimal_supervisor).

-behaviour(supervisor).

%% API
-export([start_link/1, stop/0]).
-export([start_child/1, stop_child/1]).
-export([keyfind/2]).

%% Supervisor callbacks
-export([init/1]).

-include("minimal_supervisor.hrl").

-define(Threshold, 5).

-record(child_state, {pid, id, spec}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(ChildSpecList) ->
    Pid = spawn_link(?MODULE, init, [{?TimeoutOriginalValue, []}]),
    register(?Supervisor, Pid),
    ?Supervisor ! {start_children, ChildSpecList},
    {ok, supervisor_started}.

%%--------------------------------------------------------------------
%% @doc
%% Stops the supervisor
%% @end
%%--------------------------------------------------------------------
stop() ->
    call(stop, [], 300).
    %% ?Supervisor ! {stop, self()},
    %% receive
    %% 	Reply ->
    %% 	    Reply
    %% after 300 ->
    %% 	    exit(timeout)
    %% end.

%%--------------------------------------------------------------------
%% @doc
%% Starts a child
%% @end
%%--------------------------------------------------------------------
start_child(Spec) ->
    call(start_child, Spec,  round(?TimeoutOriginalValue * 1.2)).

%    ?Supervisor ! {start_child, self(), Spec},
    %% receive
    %% 	Reply ->
    %% 	    Reply
    %% after round(?TimeoutOriginalValue * 1.2) ->
    %% 	    exit(timeout)
    %% end.

%%--------------------------------------------------------------------
%% @doc
%% Stops a child
%% @end
%%--------------------------------------------------------------------
stop_child(Id) ->
    call(stop_child, Id, 300).

    %% ?Supervisor ! {stop_child, self(), Id},
    %% receive
    %% 	Reply ->
    %% 	    Reply
    %% after 300 ->
    %% 	    exit(timeout)
    %% end.


%%--------------------------------------------------------------------
%% @doc
%% Finds Id of a child given key Pid |
%% Finds Spec of a child given key Id 
%% @end
%%--------------------------------------------------------------------
keyfind(Target, Key) ->
    call(keyfind, {Target, Key}, 300).

    %% ?Supervisor ! keyfind_internal(Target, Key),
    %% receive
    %% 	Reply ->
    %% 	    Reply
    %% after 300 ->
    %% 	    exit(timeout)
    %% end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/1,
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
init(Args) ->
    process_flag(trap_exit, true),
    loop(Args).

%%%===================================================================
%%% Internal functions
%%%===================================================================

loop({TimeoutValue, ChildList} = State) ->
    receive
	{'EXIT', _Pid, supervisor_apply_child_sto} ->
	    loop(State);
	{'EXIT', Pid, Reason} ->
	    NewChildList = restart_child(Pid, ChildList, Reason),
	    loop({TimeoutValue, NewChildList});
	{stop, From, _Args} ->
	    From ! {reply, self(), terminate_children(ChildList)};
	{start_children, ChildSpecList} ->
	    start_children(ChildSpecList),
	    loop(State);
	{start_child, From, ChildSpec} ->
	    ?Supervisor ! start_child(ChildSpec, ?Threshold, 0, From),
	    loop(State);
	{{ok, child_started, #child_state{pid=_ChildPid, id=_Id, spec=_ChildSpec} = Child} = Result, From} ->
	    NewChildList = [Child|ChildList],
	    From ! {reply, ?Supervisor, Result},
	    loop({TimeoutValue, NewChildList});
	{reply, ?Supervisor, {ok, child_started, _Child}} ->
	    loop(State);
	{{error, child_not_started, _ChildSpec} = Result, From} ->
	    From ! {reply, ?Supervisor, Result},
	    loop(State);
	{reply, ?Supervisor, {error, child_not_started, _ChildSpec}} ->
	    loop(State);
	{stop_child, From, ChildId} ->
	    ?Supervisor ! {stop_child(ChildId, ChildList), From},
	    loop(State);
	{{ok, child_stopped, ChildId, _ChildSpec} = Result, From} ->
	    NewChildList = lists:keydelete(ChildId, #child_state.id, ChildList),
	    From ! {reply, ?Supervisor, Result},
	    loop({TimeoutValue, NewChildList});
	{{error, child_not_found, _ChildId} = Result, From} ->
	    From ! {reply, ?Supervisor, Result},
	    loop(State);
	{keyfind, From, {Target, Key}} ->
	    From ! keyfind(Target, Key, ChildList),
	    loop(State);
	{timeout, NewTimeoutValue} ->
	    loop({NewTimeoutValue, ChildList})
    after TimeoutValue ->
	    exit(timeout)
    end.

call(Protocol, Args, TimeoutValue) ->
    case is_alive(?Supervisor) of
	{error, supervisor_proc_not_alive} ->
	    {error, supervisor_not_running};
	{ok, ?Supervisor, _SupervisorPid} ->
	    ?Supervisor ! {Protocol, self(), Args},
	    receive
		Reply ->
		    Reply
	    after TimeoutValue ->
		    exit(timeout)
	    end
    end.

is_alive(SupervisorName) ->
   case whereis(SupervisorName) of
       undefined ->
	   {error, supervisor_proc_not_alive};
       Pid when is_pid(Pid) ->
	   {ok, ?Supervisor, Pid}
   end.
	   
start_children([]) ->
    ?Supervisor ! {timeout, infinity};
start_children([ChildSpec|T]) ->
    ?Supervisor ! {start_child, ?Supervisor, ChildSpec},
    start_children(T).

start_child(ChildSpec, RestartThreshold, RestartThreshold, From) ->
     {{error, child_not_started, ChildSpec}, From};
start_child({_Type, {M, F, Args}} = ChildSpec, RestartThreshold, Acc, From) ->
    case apply_start_child_spec(M, F, Args) of
	{ok, Pid} ->
	    {{ok, child_started, #child_state{pid= Pid, id= erlang:system_time(millisecond), spec = ChildSpec}}, From};
	{error, _E, _Detail} ->
	    io:format("~p. last attempt to start child ~p has failed. Supervisor is trying again...~n", [Acc + 1, ChildSpec]),
	    receive after round(?TimeoutOriginalValue / ?Threshold + 1) -> ok end,
	    start_child(ChildSpec, RestartThreshold, Acc + 1, From)
    end.

apply_start_child_spec(M, F, Args) ->
    try M:F(Args) of
	{ok, Pid} ->
	    {ok, Pid}
    catch
	E:Detail ->
	    {error, E, Detail}
    end.

terminate_children([]) ->
    {ok, stopped};
terminate_children([#child_state{pid=Pid, id=_Id, spec=_Spec}|T]) ->
    apply_child_stop(Pid),
    terminate_children(T).

stop_child(Id, ChildList) ->
    case lists:keyfind(Id, #child_state.id, ChildList) of
	#child_state{pid=Pid, id= Id, spec = ChildSpec} ->
	    apply_child_stop(Pid),
	    {ok, child_stopped, Id, ChildSpec};
	false ->
	    {error, child_not_found, Id}
    end.

restart_child(Pid, ChildList, normal) ->
    case lists:keyfind(Pid, #child_state.pid, ChildList) of
	#child_state{pid=Pid, id= _Id, spec= {transient, _ChildModule}} ->
	    ChildList;
	#child_state{pid=_Pid, id=_Id, spec={permanent, {M, F, Args}}} = Child ->
	    case apply_start_child_spec(M, F, Args) of
		{ok, NewPid} ->
		    [Child#child_state{pid=NewPid}|lists:keydelete(Pid, #child_state.pid, ChildList)];
		{error, _E, _Detail} ->
		    lists:keydelete(Pid, #child_state.pid, ChildList)
	    end;
	false ->
	    ChildList
    end;
restart_child(Pid, ChildList, _Reason) ->
    case lists:keyfind(Pid, #child_state.pid, ChildList) of
	#child_state{pid = _Pid, id = _Id, spec = {_Type, {M, F, Args}}} = Child ->
	    case apply_start_child_spec(M, F, Args) of
		{ok, NewPid} ->
		    [Child#child_state{pid=NewPid}|lists:keydelete(Pid, #child_state.pid, ChildList)];
		{error, _E, _Detail} ->
		    lists:keydelete(Pid, #child_state.pid, ChildList)
	    end;
	false ->
	    ChildList
    end.

apply_child_stop(Pid) ->
    exit(Pid, supervisor_apply_child_stop).

keyfind(_Target, Key, []) ->
    {error, child_not_found, Key};
keyfind(id, Pid, ChildList) ->
    case lists:keyfind(Pid, #child_state.pid, ChildList) of
	#child_state{pid=Pid, id=Id, spec = _Spec} ->
	    {ok, child_found, Id, Pid};
	false ->
	    {error, child_not_found, Pid}
    end;
keyfind(spec, ChildId, ChildList) ->
    case lists:keyfind(ChildId, #child_state.id, ChildList) of
	#child_state{pid=_ChildPid, id=ChildId, spec= Spec} ->
	    {ok, child_found, ChildId, Spec};
	false ->
	    {error, child_not_found, ChildId}
    end;
keyfind(pid, ChildId, ChildList) ->
    case lists:keyfind(ChildId, #child_state.id, ChildList) of
	#child_state{pid=Pid, id=ChildId, spec=_Spec} ->
	    {ok, child_found, ChildId, Pid};
	false ->
	    {error, child_not_found, ChildId}
    end;
keyfind(_Target, Key, _ChildList) ->
    {error, not_supported_key, Key}.
