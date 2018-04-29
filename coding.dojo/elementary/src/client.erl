-module(client).
-export([start/1, dos/1, request/2, loop/2]).

start(MathServerPid) ->
    spawn(?MODULE, loop, [MathServerPid, self()]).

request(Client, Request) ->
    Client ! {request, self(), Request}.

dos(L) ->
    io:format("Printing ~p ~n", [L]).

loop(MathServerPid, ClientPid) ->
    receive
	{request, _From, Request} ->
	    MathServerPid ! {request, self(), Request},
	    loop(MathServerPid, ClientPid);
	{MathServerPid, ok, Reply} ->
	    io:format("Areas: ~p~n", [Reply]),
	    ClientPid ! {response, ok, Reply};
	M ->
	    ClientPid ! {response, error, M}
    end.
