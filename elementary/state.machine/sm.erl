-module(sm).
-export([start/0, stop/0, 
	 b/0, get_busy/0, a/0, get_idle/0,
	 i/0, which_state/0]).
-export([init/0]).

start() ->
    register(?MODULE, spawn(?MODULE, init, [])).

stop() ->
    ?MODULE ! stop.

b() ->
    get_busy().

get_busy() ->
    ?MODULE ! {busy, self()},
    receive
	Reply ->
	    io:format(" ~p~n", [Reply])
    after 500 ->
	    exit(timeout)
    end.

i() ->
    which_state().

which_state() ->
    ?MODULE ! {which, self()},
    receive
	Reply ->
	    io:format("State is: ~p~n", [Reply])
    after 500 ->
	    exit(timeout)
    end.

a() ->
    get_idle().

get_idle() ->
    ?MODULE ! idle.

init() ->
    idle().

idle() ->
    receive
	{busy, Pid} ->
	    busy(Pid);
	stop ->
	    exit(normal);
	{which, Pid} ->
	    Pid ! idle,
	    idle()
    end.

busy(Client) ->
    spawn(fun() -> Client ! nowoniambusy end),
    busy_loop(Client).

busy_loop(Client) ->
    receive
	{busy, Pid} ->
	    Pid ! already_busy,
	    busy_loop(Client);
	idle ->
	    idle();
	{which, Pid} ->
	    Pid ! {busywith, Client},
	    busy_loop(Client)
    end.
