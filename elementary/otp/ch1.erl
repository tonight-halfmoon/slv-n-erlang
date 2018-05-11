-module(ch1).
-export([start/0]).
-export([alloc/0, free/1]).
-export([init/0]).

start() ->
    spawn(ch1, init, []).

alloc() ->
    ch1 ! {self(), alloc},
    receive
	{ch1, Res} ->
	   Res 
	end.

free(Ch) ->
    ch1 ! {free, Ch},
    ok.

init() ->
    register(ch1, self()),
    Chs = {[], ['av']},
    loop(Chs).

loop(Chs) ->
    receive
	{From, alloc} ->
	    {Ch, Chs2} = allocate(Chs), 
	    From ! {ch1, Ch},
	    loop(Chs2);
	{free, Ch} ->
	    Chs2 = free(Ch),
	    loop(Chs2)
	end.
allocate({Allocated, [H|T] = _Free}) ->
    {H, {[H|Allocated], T}}.
