-module(math_server).
-export([start/0, stop/0, call/1,
	init/1]).

-include("math_server.hrl").

-define(NOTEST, true).
-include_lib("eunit/include/eunit.hrl").

start() ->
    register(?MathServer, spawn(?MODULE, init, [fun geometry:areas/1])).

stop() ->
    stop(?MathServer).

call(Shapes) ->
    ?MathServer ! {request, self(), Shapes},
    Reply = receive
		{response, error, Why} ->
		    {error, Why};
		{response, ok, Areas} ->
		    Areas
	    end,
    Reply.

init(F) ->
    loop(F).

stop(RegName) ->
    stop(whereis(RegName), RegName).

stop(undefined, _RegName) ->
    ok;
stop(Pid, RegName) ->
    case is_process_alive(Pid) of
	true ->
	    send_stop_protocol(RegName);
	false ->
	    ok
    end.

send_stop_protocol(RegName) ->
   RegName ! stop.

loop(F) ->
    receive
	stop ->
            exit(normal);
	{request, From, Query} ->
	    case catch F(Query) of
		Areas when is_float(Areas); is_integer(Areas) ->
		    From ! {response, ok, F(Query)},
		    loop(F);
		Bad ->
		    From ! {response, error, Bad},
		    loop(F)
	    end
    after 3000 ->
	    io:format("Timeout. Server shutdown.~n", []),
	    exit(timeout)
	end.
