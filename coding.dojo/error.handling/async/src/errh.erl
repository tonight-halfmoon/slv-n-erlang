-module(errh).
-export([start/1, stop/1]).
-export([increment/1]).
-export([init/1, cast/1]).

start(Args) ->
    Pid = spawn(?MODULE, init, [Args]),
    {ok, Pid}.

stop(Pid) ->
    Pid ! {stop, self()},
    {ok, noreply}.

increment({N, Pid}) ->
    spawn(?MODULE, cast, [{inc, self(), N, Pid}]),
    {ok, noreply}.

init({InitialState, Pid}) when is_pid(Pid) ->
    process_flag(trap_exit, true),
    link(Pid),
    loop(InitialState);
init(InitialState) ->
    process_flag(trap_exit, true),
    loop(InitialState).

cast({inc, ClientPid, Payload, Pid}) ->
    Pid ! {inc, self(), Payload},
    receive
	{'EXIT', _From, Reason} ->
	    exit(Reason);
        Reply ->
	    ClientPid ! Reply
    end.

loop(State) ->
    receive
	{'EXIT', _From, Reason} ->
	    exit(Reason);
	{stop, _From} ->
	    exit(normal);
	{inc, From, Payload} ->
	    NewState = compute({sum, State, Payload, 1}),
	    From ! {reply, self(), NewState},
	    loop(NewState)
	end.

compute({sum, State, N, Inc}) ->
    case catch State + N + Inc of
	{'EXIT', Reason} ->
	    exit(Reason);
	Sum ->
	    Sum
    end.
