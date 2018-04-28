-module(math_server).
-export([start/0, stop/0, call/1,
	init/1]).

-include("math_server.hrl").

-define(NOTEST, true).
-include_lib("eunit/include/eunit.hrl").

start() ->
    register(?MathServer, spawn(?MODULE, init, [fun geometry:areas/1])).

stop() ->
    shutdown_math_server().

call(Shapes) ->
    ?MathServer ! {request, self(), Shapes},
    Reply = receive
		{response, ok, Areas} ->
		    Areas
	    end,
    Reply.

shutdown_math_server() ->
    case whereis(?MathServer) of
    	undefined -> 
    	    true;
    	Pid ->
    	    case is_process_alive(Pid) of
    		true ->
    		    ?MathServer ! stop,
    		    unregister(?MathServer);
    		false ->
    		    unregister(?MathServer)
    	    end
    end.

init(F) ->
    loop(F).

loop(F) ->
    receive
	stop ->
            exit(normal);
	{request, From, Query} ->
	    From ! {response, ok, F(Query)},
	    loop(F)
    after 1500000 ->
	    io:format("Timeout. Server shutdown.~n", []),
	    exit(timeout)
	end.
