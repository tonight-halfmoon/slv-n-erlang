-module(server).
-export([start/0, stop/0]).
-export([stats/0]).
-export([init/1]).
-include("server.hrl").

start() ->
    Pid = spawn(?MODULE, init, [fun geometry:areas/1]),
    register(?Server, Pid),
    {ok, noreply}.

stop() ->
    ?Server ! {stop, self()},
    {ok, noreply}.

stats() ->
    ?Server ! {request, self(), stats},
    receive
	Reply ->
	    Reply
    end.

init(Args) ->
    process_flag(trap_exit, true),
    InitialState = initialise_resources(),
    loop(InitialState, Args).

initialise_resources() ->
    [{4, undefined}, {9, undefined}].%, {14, undefined}, {5,undefined}].

loop(State, Callback) ->
    receive
	{'EXIT', From, _Why} ->
	    NewState = disconnect_client(State, From),
	    loop(NewState, Callback);
	{stop, _From} ->
	    deallocate_all(),
	    unregister(?Server),
	    exit(stop);
	{sum_areas, Shapes, Pid} ->
	    Result = eval(Callback, Shapes),
	    Pid ! {reply, Result},
	    loop(State, Callback);
	{connect, ClientPid} ->
	    case connect_client(State, ClientPid) of
		{error, exhausted} = Reply ->
		    ClientPid ! {reply, ?Server, {Reply, not_connected}},
		    loop(State, Callback);
		NewState ->
		    link(ClientPid),
		    ClientPid ! {reply, ?Server, connected},
		    loop(NewState, Callback)
	    end;
	{disconnect, ClientPid} ->
	    NewState = disconnect_client(State, ClientPid),
	    unlink(ClientPid),
	    ClientPid ! {reply, ?Server, disconnected},
	    loop(NewState, Callback);
	{request, From, stats} ->
	    From ! {reply, State},
	    loop(State, Callback)
    end.

eval(Fun, Args) ->
    case catch Fun(Args) of
	{'EXIT', Why} ->
	    {error, Why};
	Sum ->
	    {ok, Sum}
    end.

connect_client(Resources, ClientPid) ->
    allocate(Resources, ClientPid).

disconnect_client(Resources, ClientPid) ->
    deallocate(Resources, ClientPid).

deallocate_all() ->
    initialise_resources().

allocate(Resources, Pid) ->
    Result = case lists:keyfind(Pid, 2, Resources) of
		 {_Res, Pid} ->
		     {error, already_allocated};
		 false ->
		     case lists:keyfind(undefined, 2, Resources) of
			 {Res, undefined} ->
			     lists:keyreplace(undefined, 2, Resources, {Res, Pid});
			 false ->
			     {error, exhausted}
		     end
	     end,
    Result.

deallocate(Resources, Pid) ->
    Result = case lists:keyfind(Pid, 2, Resources) of
		 {Res, Pid} ->
		     lists:keyreplace(Pid, 2, Resources, {Res, undefined});
		 false ->
		     {error, not_allocated}
	     end,
    Result.
