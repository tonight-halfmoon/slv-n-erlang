-module(errh).
-export([start/1, stop/1]).
-export([init/1]).

start(Args) ->
    Pid = spawn(?MODULE, init, [Args]),
    {ok, Pid}.

stop(Pid) ->
    Pid ! stop,
    {ok, noreply}.

init({InitialState, Pid}) ->
    process_flag(trap_exit, true),
    link(Pid),
    loop(InitialState);
init(InitialState) ->
    process_flag(trap_exit, true),
    loop(InitialState).

loop(_State) ->
    receive
	{'EXIT', _From, Reason} ->
	    exit(Reason);
	stop ->
	    exit(normal)
    end.
