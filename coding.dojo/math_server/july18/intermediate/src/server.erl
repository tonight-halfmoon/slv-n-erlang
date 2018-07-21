-module(server).
-export([start/0, stop/1]).
-export([init/1]).

start() ->
    Pid = spawn(?MODULE, init, [fun geometry:areas/1]),
    {ok, Pid}.

stop(Pid) ->
    Pid ! {stop, self()},
    {ok, noreply}.

init(InitialState) ->
    loop(InitialState).

loop(Callback) ->
    receive
	{stop, _From} ->
	    exit(stop);
	{sum_areas, Shapes, Pid} ->
	    Result = eval(Callback, Shapes),
	    Pid ! {reply, Result},
	    loop(Callback)
    end.

eval(Fun, Args) ->
    case catch Fun(Args) of
	{'EXIT', Why} ->
	    {error, Why};
	Sum ->
	    {ok, Sum}
    end.

