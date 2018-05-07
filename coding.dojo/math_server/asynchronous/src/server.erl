-module(server).
-export([start/0, sum_areas/2, stop/1,
	 loop/1, async_client/3]).

start() ->
    Pid = spawn(?MODULE, loop, [fun geometry:areas/1]),
    {ok, Pid}.

sum_areas(Shapes, ServerPid) ->
    Pid = spawn(?MODULE, async_client, [self(), Shapes, ServerPid]),
    {ok, Pid}.

stop(ServerPid) ->
    ServerPid ! stop,
    {ok, stopped}.

loop(F) ->
    receive
	{request, Client, {sum_areas, Shapes}} ->
	    Result = eval(F, Shapes),
	    Client ! {reply, self(), Result},
	    loop(F);
	stop ->
	    ok
    end.

eval(F, Shapes) ->
    case catch F(Shapes) of
	{'EXIT', Why} ->
	    {error, Why};
	Sum ->
	    {ok, Sum}
    end.

async_client(ClientPid, Shapes, ServerPid) ->
    ServerPid ! {request, self(), {sum_areas, Shapes}},
    receive
	{reply, ServerPid, Result} ->
	    ClientPid ! Result,
	    exit(normal)
    end.
