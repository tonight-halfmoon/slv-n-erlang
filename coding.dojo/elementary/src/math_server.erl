-module(math_server).
-export([start/0, loop/1, call/2]).

start() ->
    spawn(?MODULE, loop, [fun geometry:areas/1]).

call(MathServerPid, Message) ->
    MathServerPid ! Message,
    receive {reply, ok, Areas} ->
	    Areas
   end.

loop(F) ->
    receive
	{request, From, Shapes} ->
	    From ! {reply, ok, F(Shapes)},
	    loop(F)
    end.
