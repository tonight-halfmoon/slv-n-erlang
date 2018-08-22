-module(client).
-export([start/0, start/1, stop/0, stop/1]).
-export([connect/0, connect/1, disconnect/0, disconnect/1]).
-export([sum_areas/1, async_sum_areas/1]).
-export([is_connected/0, is_connected/1]).
-export([init/1]).
-include("server.hrl").
-include("client.hrl").

start() ->
    start(?Client).

start(Name) ->
    Pid = spawn(?MODULE, init, [{}]),
    register(Name, Pid),
    {ok, Pid}.

connect() ->
    connect(?Client).

connect(Name) ->
    Name ! {connect, self()},
    receive
	Reply ->
	    Reply
    after 300 ->
	    exit(timeout)
    end.

disconnect() ->
    disconnect(?Client).

disconnect(Name) ->
    Name ! {disconnect, self()},
    receive
	Reply ->
	    Reply
    after 300 ->
	    exit(timeout)
    end.

is_connected() ->
    is_connected(?Client).

is_connected(Name) ->
    Name ! {is_connected, self()},
    receive
	Reply ->
	    Reply
    after 300 ->
	    exit(timeout)
    end.

stop() ->
    stop(?Client).

stop(Name) ->
    Name ! {stop, self()},
    receive
     	Reply ->
    	    Reply
    after 300 ->
    	    exit(timeout)
    end.

sum_areas(Shapes) ->
    ?Client ! {sum_areas, Shapes, self()},
    receive
	Reply ->
	    Reply
    after 500 ->
	    exit(timeout)
    end.

async_sum_areas(Shapes) ->
    TrueClient = self(),
    spawn(fun() -> ?Client ! {sum_areas, Shapes, TrueClient} end),
    {ok, noreply}.

init(InitialState) ->
    loop(InitialState).

loop(State) ->
    receive
	{stop, From} ->
	    io:format("Client received ~p from ~p.~nStopping...~n", [stop, From]),
	    disconnect(self()),
	    From ! {ok, self(), stopped};
	{connect, From} ->
	    ?Server ! {connect, From, self()},
	    loop(not_yet);
	{sum_areas, Shapes, TrueClient} ->
	    ?Server ! {sum_areas, Shapes, TrueClient},
	    loop(State);
	{disconnect, From} ->
	    ?Server ! {disconnect, From, self()},
	    loop(State);
	{reply, ?Server, {ok, connected}} ->
	    loop({yes, connnected});
	{reply, ?Server, {ok, _Topic, _Info}} ->
	    loop(State);
	{reply, ?Server, {ok, disconnected}} ->
	    loop({no, disconnected});
	{reply, ?Server, {error, _Topic, _Info}} ->
	    loop(State);
	{is_connected, From} ->
	    From ! {reply, self(), State},
	    loop(State);
        Reply ->
	    io:format("Client received ~p.~nTerminating...~n", [Reply]),
	    {client, self(), terminated}
    end.
