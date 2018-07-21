-module(server).
-export([start/0, stop/1, sum_areas/2, async_sum_areas/2]).
-export([init/1]).

start() ->
    Pid = spawn(?MODULE, init, [fun geometry:areas/1]),
    {ok, Pid}.

stop(Pid) ->
    Pid ! {stop, self()},
    {ok, noreply}.

sum_areas(Shapes, ServerPid) -> 
    ServerPid ! {sum_areas, Shapes, self()},
    receive
	Reply ->
	    Reply
    end.

async_sum_areas(Shapes, ServerPid) ->
    ClientPid = self(),
    spawn(fun() -> ServerPid ! {sum_areas, Shapes, ClientPid} end),
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
