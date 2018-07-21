-module(server).
-export([start/0, stop/0]).
-export([init/1]).
-include("server.hrl").

start() ->
    Pid = spawn(?MODULE, init, [fun geometry:areas/1]),
    register(?Server, Pid),
    {ok, Pid}.

stop() ->
    ?Server ! {stop, self()},
    {ok, noreply}.

init(InitialState) ->
    loop(InitialState).

loop(Callback) ->
    receive
	{stop, _From} ->
	    unregister(?Server),
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

