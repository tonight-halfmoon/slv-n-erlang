-module(math_server).
-export([start/0, loop/1, rpc/2]).

start() ->
    register(?MODULE, spawn(?MODULE, loop, [fun geometry:areas/1])).

rpc(?MODULE, {request, Pid, Query}) ->
    ?MODULE ! {request, Pid, Query},
    receive
	{?MODULE, ok, Reply} ->
	    Pid ! {?MODULE, ok, Reply}
	end.

loop(F) ->
    receive
	{request, From, Query} ->
	    From ! {?MODULE, ok, F(Query)},
	    loop(F)
    after 1500000 ->
	    io:format("Timeout. Server shutdown.~n", []),
	    exit(timeout)
	end.


% Extra on Emulator
% 1> math_server:rpc(math_server, {request, spawn(fun () -> receive M -> io:format("Fun received ~p~n", [M]) end end), [{circle, 3}]}).
