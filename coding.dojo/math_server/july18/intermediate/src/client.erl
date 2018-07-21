-module(client).
-export([sum_areas/2, async_sum_areas/2]).

sum_areas(Shapes, ServerPid) -> 
    ServerPid ! {sum_areas, Shapes, self()},
    receive
	Reply ->
	    Reply
    after 500 ->
	exit(timeout)
    end.

async_sum_areas(Shapes, ServerPid) ->
    ClientPid = self(),
    spawn(fun() -> ServerPid ! {sum_areas, Shapes, ClientPid} end),
    {ok, noreply}.

