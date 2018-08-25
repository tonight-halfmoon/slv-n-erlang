-module(server).
-export([start_link/0, start_link/1, stk/1]).
-export([check_resources/0]).
-export([init/1]).

-include("server.hrl").

stk(Args) ->
    start_link(Args).

start_link() ->
    start_link(available_resources()).

start_link(Args) ->
    Pid = spawn_link(?MODULE, init, [{Args, fun geometry:areas/1}]),
    register(?Server, Pid),
    {ok, Pid}.

check_resources() ->
    ?Server ! {request, self(), check_resources},
    receive
	Reply ->
	    Reply
    after 500 ->
	    exit(timeout)
    end.

init({InitialState, Callback}) ->
    process_flag(trap_exit, true),
    loop(InitialState, Callback).

loop(State, Callback) ->
    receive
	{'EXIT', _From, normal} ->
	    {ok, self(), stopped_normally};
	{'EXIT', From, kill} ->
	    {ok, self(), killed_by, From};
	{'EXIT', From, _Reason} ->
	    case disconnect_client(State, From) of
		{State, {error, not_allocated} = Result} ->
		    From ! {reply, self(), Result};
		{NewState, {ok, disconnected} = Result} ->
		    From ! {reply, self(), Result},
		    loop(NewState, Callback)
	    end;
        {stop, _From} ->
	    deallocate_all();
	{connect, From, Client} ->
	    {NewState, Reply} = connect_client(State, Client),
	    From ! {reply, ?Server, Reply},
	    loop(NewState, Callback);
	{disconnect, From, Client} ->
	    {NewState, Reply} = disconnect_client(State, Client),
	    From ! {reply, ?Server, Reply},
	    loop(NewState, Callback);
	{sum_areas, Shapes, From} ->
	    Result = eval(Callback, Shapes),
	    From ! {reply, ?Server, Result},
	    loop(State, Callback);
	{request, From, check_resources} ->
	    From ! {reply, ?Server, State},
	    loop(State, Callback)
    end.

eval(Fun, Args) ->
    case catch Fun(Args) of
	{'EXIT', Why} ->
	    {error, Why};
	Sum ->
	    {ok, Sum}
    end.

deallocate_all() ->
    available_resources().

available_resources() ->
    [{'0x41a', undefined}, {'0xf23', undefined}, {'0xe93', undefined}].

connect_client(State, ClientPid) ->
    case allocate(State, ClientPid) of
	{error, exhausted} ->
	    {State, {error, connect, 'no enough resources'}};
	{error, already_allocated} = ALC ->
	    {State, ALC};
	{ok, NewAllocations} ->
	    link(ClientPid),
	    {NewAllocations, {ok, connected}}
    end.

disconnect_client(State, ClientPid) ->
    case deallocate(State, ClientPid) of
	{error, not_allocated} = NALC ->
	    {State, NALC};
	{ok, NewAllocations} ->
	    unlink(ClientPid),
	    {NewAllocations, {ok, disconnected}}
    end.

allocate(Resources, Pid) ->
    Result = case lists:keyfind(Pid, 2, Resources) of
		 {_Res, Pid} ->
		     {error, already_allocated};
		 false ->
		     case lists:keyfind(undefined, 2, Resources) of
			 {Res, undefined} ->
			     NewAllocations = lists:keyreplace(undefined, 2, Resources, {Res, Pid}),
			     {ok, NewAllocations};
			 false ->
			     {error, exhausted}
		     end
	     end,
    Result.

deallocate(Resources, Pid) ->
    Result = case lists:keyfind(Pid, 2, Resources) of
		 {Res, Pid} ->
		     NewAllocations = lists:keyreplace(Pid, 2, Resources, {Res, undefined}),
		     {ok, NewAllocations};
		 false ->
		     {error, not_allocated}
	     end,
    Result.
