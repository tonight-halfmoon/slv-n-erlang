-module(server).
-export([start/0, stop/0, sum_areas/1,
	init/1]).

-include("server.hrl").

-define(NOTEST, true).
-include_lib("eunit/include/eunit.hrl").

start() ->
    start(?math_server).

stop() ->
    stop(?math_server).

sum_areas(Shapes) ->
    call({sum_areas, Shapes}).

call({sum_areas, Shapes}) ->
    ?math_server ! {request, self(), {sum_areas, Shapes}},
    receive
	{reply, {sum_areas, error, Why}} ->
	    {error, Why};
	{reply, {sum_areas, ok, Areas}} ->
	    {ok, Areas}
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
	{request, From, {sum_areas, Shapes}} ->
	    case eval(F, Shapes) of
		{ok, Result}->
		    From ! {reply, {sum_areas, ok, Result}},
		    loop(F);
		{error, Why}->
		    From ! {reply, {sum_areas, error, Why}},
		    loop(F)
	    end
    after 40000 ->
	    io:format("Timeout. Server shutdown.~n", []),
	    exit(timeout)
    end.

eval(Fun, Args) ->
    case catch Fun(Args) of
	{'EXIT', Why} ->
	    {error, Why};
	Result ->
	    {ok, Result}
    end.
