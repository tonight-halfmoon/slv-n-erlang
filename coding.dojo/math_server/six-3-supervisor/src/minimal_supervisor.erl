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
%-export([stop_child/1]).

%% Supervisor callbacks
-export([init/1]).

-include("minimal_supervisor.hrl").

-define(Threshold, 5).

-define(StartChildTimeout, 6000).

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
	    io:format("restarting child killed"),
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
	{ok, ChildPid, ChildSpec} ->
	    io:format("Child for Spec '~p' is started.~n", [ChildSpec]),
	    NewChildList = [{ChildPid, ChildSpec}|ChildList],
	    loop({Timeout, NewChildList});
	{error, child_not_started, ChildSpec} ->
	    io:format("Child for spec ~p cannot be started.~n", [ChildSpec]),
	    loop({Timeout, ChildList});
	{start_child, ChildSpec} ->
	    start_child(ChildSpec, ?Threshold, 0),
	    loop({Timeout, ChildList});
	{timeout, NewTimeout} ->
	    loop({NewTimeout, ChildList})
    after Timeout ->
	    exit(timeout)
    end.

start_children([]) ->
    ?Supervisor ! {timeout, infinity};
start_children([ChildSpec|T]) ->
    self() ! {start_child, ChildSpec},
    start_children(T).

start_child({_Type, {M, F, Args}} = ChildSpec) ->
    try M:F(Args) of
	{ok, Pid} ->
	    {ok, Pid, ChildSpec}
    catch
	_E:_Detail ->
	    {error, child_not_started, ChildSpec}
    end.

start_child(ChildSpec, Threshold, Threshold) ->
    ?Supervisor ! {error, child_not_started, ChildSpec};
start_child(ChildSpec, Threshold, Acc) ->
    case start_child(ChildSpec) of
	{ok, Pid, ChildSpec} ->
	    ?Supervisor ! {ok, Pid, ChildSpec};
	{error, child_not_started, ChildSpec} ->
	    io:format("~p. Child for Spec '~p' was not started. Supervisor is trying again.~n", [Acc + 1, ChildSpec]),
	    receive after round(?StartChildTimeout / ?Threshold) -> ok end,
	    start_child(ChildSpec, Threshold, Acc + 1)
    end.

restart_child(Pid, ChildList, normal) ->
    case lists:keyfind(Pid, 1, ChildList) of
	{Pid, {transient, _ChildModule}} ->
	    ChildList;
	{Pid, {permanent, _ChildModule} = ChildSpec} ->
	    case start_child(ChildSpec) of
		{ok, NewPid} ->
		    [{NewPid, ChildSpec}|lists:keydelete(Pid, 1, ChildList)];
		{error, child_not_started, ChildSpec} ->
		    lists:keydelete(Pid, 1, ChildList)
	    end;
	false ->
	    ChildList
    end;
restart_child(Pid, ChildList, _Reason) ->
    case lists:keyfind(Pid, 1, ChildList) of
	{Pid, ChildSpec} ->
	      case start_child(ChildSpec) of
		{ok, NewPid, ChildSpec} ->
		    [{NewPid, ChildSpec}|lists:keydelete(Pid, 1, ChildList)];
		{error, child_not_started, ChildSpec} ->
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
