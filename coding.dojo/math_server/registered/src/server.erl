-module(server).
-export([start/0, stop/0, call/1,
	init/1]).

-include("server.hrl").

-define(NOTEST, true).
-include_lib("eunit/include/eunit.hrl").

start() ->
    start(?math_server).

stop() ->
    stop(?math_server).

call(Shapes) ->
    ?math_server ! {request, self(), Shapes},
    receive
	{response, error, Why} ->
	    {error, Why};
	{response, ok, Areas} ->
	    Areas
    after 50 ->
	    exit(timeout)
    end.

init(F) ->
    loop(F).

start(Name) ->
    case whereis(Name) of
	undefined ->
	    Pid = spawn(?MODULE, init, [fun geometry:areas/1]),
	    register(Name, Pid),
	    {ok, Pid};
	Pid when is_pid(Pid) ->
	    {error, already_started}
    end.

stop(Name) ->
    stop(whereis(Name), Name).

stop(undefined, _Name) ->
    ok;
stop(Pid, Name) ->
    case is_process_alive(Pid) of
	true ->
	    send_stop_protocol(Name);
	false ->
	    ok
    end.

send_stop_protocol(Name) ->
    Name ! stop.

loop(F) ->
    receive
	stop ->
            exit(shutdown);
	{request, From, Query} ->
	    case catch F(Query) of
		Areas when is_float(Areas); is_integer(Areas) ->
		    From ! {response, ok, F(Query)},
		    loop(F);
		Result ->
		    From ! {response, error, Result},
		    loop(F)
	    end
    after 90000 ->
	    io:format("Timeout. Server shutdown.~n", []),
	    exit(timeout)
    end.
