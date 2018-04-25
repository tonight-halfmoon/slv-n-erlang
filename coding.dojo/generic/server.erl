-module(server).
-export([start/1, loop/1, rpc/2]).

start(F) ->
    process_flag(trap_exit, true),
    spawn(?MODULE, loop, [F]).

rpc(Server, {From, Query}) ->
    Server ! {request, From, Query},
    receive
	{reply, ok, Response} ->
	    Response
    end.

loop(F) ->
    receive
	{'EXIT', From, Why} ->
	    {error, Why};
	{request, From, Query} ->
	    From ! {reply, ok, F(Query)},
	    loop(F);
	M ->
	    io:format("Received ~p~n", [M])
    after 360000 ->
	    io:format("Timeout. Server shutdown.~n", []),
	    exit(timeout)
	end.

%% 1> server:start(fun geometry:areas/1).
%% <0.63.0>
%% 2> Server = server:start(fun geometry:areas/1). 
%% <0.65.0>
%% 3> Server ! {request, spawn(fun() -> receive M -> io:format("Fun received ~p~n", [M]) end end ) , [{circle, 3}]}.
%% {request,<0.67.0>,[{circle,3}]}
%% Fun received {reply,ok,28.274333882308138}
%% 4> Server ! {request, spawn(fun() -> receive M -> io:format("Fun received ~p~n", [M]) end end ) , [{circle, 3}]}.
%% Fun received {reply,ok,28.274333882308138}
%% {request,<0.69.0>,[{circle,3}]}
%% 5> Server ! {request, spawn(fun() -> receive M -> io:format("Fun received ~p~n", [M]) end end ) , [{circle, 32}]}.
%% Fun received {reply,ok,3216.990877275948}
%% {request,<0.71.0>,[{circle,32}]}
%% 6> 
