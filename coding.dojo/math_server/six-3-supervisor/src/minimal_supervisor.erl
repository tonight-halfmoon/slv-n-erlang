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

%% Supervisor callbacks
-export([init/1]).

-include("minimal_supervisor.hrl").

-define(Threshold, 5).

-define(StartChildTimeout, 6000).

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
    Pid = spawn_link(?MODULE, init, [{?StartChildTimeout, []}]),
    register(?Supervisor, Pid),
    Pid ! {start_children, ChildSpecList},
    {ok, Pid}.

stop() ->
    ?Supervisor ! {stop, self()},
    receive
    	Reply ->
    	    Reply
    after 300 ->
    	    exit(timeout)
    end.

start_child(Spec) ->
    ?Supervisor ! {start_child, self(), Spec},
    receive
	Reply ->
	    Reply
    after ?StartChildTimeout ->
	    exit(timeout)
    end.

stop_child(Id) ->
    ?Supervisor ! {stop_child, self(), Id},
    receive
	Reply ->
	    Reply
    after 300 ->
	    exit(timeout)
    end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init(InitialState) ->
    process_flag(trap_exit, true),
    loop(InitialState).

%%%===================================================================
%%% Internal functions
%%%===================================================================

loop({Timeout, ChildList}) ->
    receive
	{'EXIT', Pid, normal} ->
	    NewChildList = restart_child(Pid, ChildList, normal),
	    loop({Timeout, NewChildList});
	{'EXIT', Pid, killed} ->
	    io:format("restarting child killed.~n"),
	    NewChildList = restart_child(Pid, ChildList, killed),
	    loop({Timeout, NewChildList});
	{'EXIT', Pid, Reason} ->
	    NewChildList = restart_child(Pid, ChildList, Reason),
	    loop({Timeout, NewChildList});
	{stop, From} ->
	    From ! {reply, self(), terminate_children(ChildList)};
	{start_children, ChildSpecList} ->
	    start_children(ChildSpecList),
	    loop({Timeout, ChildList});
	{ok, child_started, Child, From} ->
	    io:format("Child for Spec '~p' is started.~n", [Child#child_state.spec]),
	    From ! {reply, ?Supervisor, {ok, Child#child_state.id, Child#child_state.pid}},
	    NewChildList = [Child|ChildList],
	    loop({Timeout, NewChildList});
	{error, child_not_started, ChildSpec, From} ->
	    io:format("Child for spec ~p cannot be started.~n", [ChildSpec]),
	    From ! {reply, ?Supervisor, {error, child_not_started, ChildSpec}},
	    loop({Timeout, ChildList});
	{start_child, From, ChildSpec} ->
	    start_child(ChildSpec, ?Threshold, 0, From),
	    loop({Timeout, ChildList});
	{stop_child, From, ChildId} ->
	    {NewChildList, ChildStopResult} = stop_child_internal(ChildId, ChildList),
	    From ! {reply, ?Supervisor, ChildStopResult},
	    loop({Timeout, NewChildList});
	{timeout, NewTimeout} ->
	    loop({NewTimeout, ChildList})
    after Timeout ->
	    exit(timeout)
    end.

start_children([]) ->
    ?Supervisor ! {timeout, infinity};
start_children([ChildSpec|T]) ->
    self() ! {start_child, ?Supervisor, ChildSpec},
    start_children(T).

start_child_internal({_Type, {M, F, Args}} = ChildSpec) ->
    try M:F(Args) of
	{ok, Pid} ->
	    {ok, Pid, ChildSpec}
    catch
	_E:_Detail ->
	    {error, child_not_started, ChildSpec}
    end.

start_child(ChildSpec, Threshold, Threshold, From) ->
    ?Supervisor ! {error, child_not_started, ChildSpec, From};
start_child(ChildSpec, Threshold, Acc, From) ->
    case start_child_internal(ChildSpec) of
	{ok, Pid, ChildSpec} ->
	    ?Supervisor ! {ok, child_started, #child_state{pid= Pid, id= erlang:system_time(millisecond), spec = ChildSpec}, From};
	{error, child_not_started, ChildSpec} ->
	    io:format("~p. Child for Spec '~p' was not started. Supervisor is trying again.~n", [Acc + 1, ChildSpec]),
	    receive after round(?StartChildTimeout / ?Threshold) -> ok end,
	    start_child(ChildSpec, Threshold, Acc + 1, From)
    end.

stop_child_internal(Id, ChildList) ->
    case lists:keyfind(Id, #child_state.id, ChildList) of
	#child_state{pid=Pid, id= Id, spec = ChildSpec} ->
	    terminate_child(Pid),
	    {lists:keydelete(Id, #child_state.id, ChildList), {ok, child_stopped, ChildSpec}};
	false ->
	    {ChildList, {error, child_not_found, Id}}
    end.

restart_child(Pid, ChildList, normal) ->
    case lists:keyfind(Pid, #child_state.pid, ChildList) of
	#child_state{pid=Pid, id= _Id, spec= {transient, _ChildModule}} ->
	    ChildList;
	#child_state{pid=_Pid, id=_Id, spec={permanent, _ChildModule} = ChildSpec} = Child ->
	    case start_child_internal(ChildSpec) of
		{ok, NewPid} ->
		    [Child#child_state{pid=NewPid}|lists:keydelete(Pid, #child_state.pid, ChildList)];
		{error, child_not_started, ChildSpec} ->
		    lists:keydelete(Pid, 1, ChildList)
	    end;
	false ->
	    ChildList
    end;
restart_child(Pid, ChildList, _Reason) ->
    case lists:keyfind(Pid, #child_state.pid, ChildList) of
	#child_state{pid = _Pid, id = _Id, spec = ChildSpec} = Child ->
	      case start_child_internal(ChildSpec) of
		{ok, NewPid, ChildSpec} ->
		    [Child#child_state{pid=NewPid}|lists:keydelete(Pid, #child_state.pid, ChildList)];
		{error, child_not_started, ChildSpec} ->
		    lists:keydelete(Pid, 1, ChildList)
	    end;
	false ->
	    ChildList
    end.

terminate_children([]) ->
    {ok, children_terminated};
terminate_children([#child_state{pid=Pid, id=_Id, spec=_ChildSpec}|T]) ->
    terminate_child(Pid),
    terminate_children(T).

terminate_child(Pid) ->
    exit(Pid, supervisor_terminate_children).
