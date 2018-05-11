-module(sc_app).

-behaviour(application).

-export([start/2, stop/1]).

start(StartType, StartArgs) ->
    io:format("StartType:~w; StartArgs:~w~n", [StartType, StartArgs]),
    case sc_sup:start_link() of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    Error
	end.

stop(_State) ->
    ok.
