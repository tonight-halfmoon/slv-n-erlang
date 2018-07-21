-module(client).
-export([sum_areas/1, async_sum_areas/1]).
-include("server.hrl").

sum_areas(Shapes) ->
    ?Server ! {sum_areas, Shapes, self()},
    receive
	Reply ->
	    Reply
    after 500 ->
	exit(timeout)
    end.

async_sum_areas(Shapes) ->
    ClientPid = self(),
    spawn(fun() -> ?Server ! {sum_areas, Shapes, ClientPid} end),
    {ok, noreply}.

