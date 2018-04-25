-module(math_server).
-export([start/0, init/1]).

start() ->
    spawn(?MODULE, init, [fun geometry:areas/1]).

init(F) ->
    process_flag(trap_exit, true),
    loop(F).

loop(F) ->
    receive
	{'EXIT', From, Why} ->
	    {error, Why};
	{request, From, Query} ->
	    From ! {self(), ok, F(Query)},
	    loop(F)
    after 1500000 ->
	    io:format("Timeout. Server shutdown.~n", []),
	    exit(timeout)
    end.
