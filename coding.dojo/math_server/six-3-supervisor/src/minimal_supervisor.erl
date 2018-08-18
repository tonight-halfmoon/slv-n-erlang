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

%% Supervisor callbacks
-export([init/1]).

-include("minimal_supervisor.hrl").

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
    Pid = spawn_link(?MODULE, init, [ChildSpecList]),
    register(?Supervisor, Pid),
    {ok, Pid}.

stop() ->
    ?Supervisor ! {stop, self()},
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
init(ChildSpecList) ->
    process_flag(trap_exit, true),
    ChildProcList = start_chidlren(ChildSpecList),
    loop(ChildProcList).

%%%===================================================================
%%% Internal functions
%%%===================================================================

loop(ChildList) ->
    receive
	{'EXIT', Pid, normal} ->
	    NewChildList = restart_child(Pid, ChildList, normal),
	    loop(NewChildList);
	{'EXIT', Pid, killed} ->
	    NewChildList = restart_child(Pid, ChildList, killed),
	    loop(NewChildList);
	{'EXIT', Pid, Reason} ->
	    NewChildList = restart_child(Pid, ChildList, Reason),
	    loop(NewChildList);
	{stop, From} ->
	    From ! {reply, self(), terminate_children(ChildList)}
    after 6000 ->
	    exit(timeout)
    end.

start_chidlren(ChildSpecList) ->
    start_children(ChildSpecList, []).

start_children([], ChildList) ->
    ChildList;
start_children([{Type, ChildSpec}|T], ChildList) ->
    case start_child(ChildSpec, 5) of
	{ok, Pid} ->
	    io:format("Child for Spec '~p' is started.~n", [ChildSpec]),
	    start_children(T, [{Pid, {Type, ChildSpec}}|ChildList]);
	{error, child_not_started} ->
	    start_children(T, ChildList)
    end.

start_child({M, F, Args}) ->
    try M:F(Args) of
	{ok, Pid} ->
	    {ok, Pid}
    catch
	_E:_Detail ->
	    {error, child_not_started}
    end.

start_child(ChildSpec, Threshold) ->
    start_child(ChildSpec, Threshold + 1, 1).

start_child(_ChildSpec, Threshold, Threshold) ->
    {error, child_not_started};
start_child(ChildSpec, Threshold, Acc) ->
    case start_child(ChildSpec) of
	{ok, Pid} ->
	    {ok, Pid};
	{error, child_not_started} ->
	    io:format("~p. A child with Spec '~p' has not started yet. Supervisor is trying to start the child again.~n", [Acc, ChildSpec]),
	    receive after 120 -> ok end,
	    start_child(ChildSpec, Threshold, Acc + 1)
    end.

restart_child(Pid, ChildList, normal) ->
    case lists:keyfind(Pid, 1, ChildList) of
	{Pid, {transient, _ChildSpec}} ->
	    ChildList;
	{Pid, {permanent, ChildSpec}} ->
	    case start_child(ChildSpec) of
		{ok, NewPid} ->
		    [{NewPid, {permanent, ChildSpec}}|lists:keydelete(Pid, 1, ChildList)];
		{error, child_not_started} ->
		    lists:keydelete(Pid, 1, ChildList)
	    end;
	false ->
	    ChildList
    end;
restart_child(Pid, ChildList, _Reason) ->
    case lists:keyfind(Pid, 1, ChildList) of
	{Pid, {Type, ChildSpec}} ->
	      case start_child(ChildSpec) of
		{ok, NewPid} ->
		    [{NewPid, {Type, ChildSpec}}|lists:keydelete(Pid, 1, ChildList)];
		{error, child_not_started} ->
		    lists:keydelete(Pid, 1, ChildList)
	    end;
	false ->
	    ChildList
    end.

terminate_children([]) ->
    {ok, children_terminated};
terminate_children([{Pid, _ChildSpec}|T]) ->
    exit(Pid, supervisor_terminate_children),
    terminate_children(T).
