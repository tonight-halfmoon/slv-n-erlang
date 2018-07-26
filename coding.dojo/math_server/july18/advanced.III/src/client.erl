-module(client).
-export([sum_areas/1, async_sum_areas/1]).
-export([start/0, start/1]).
-export([connect/0, connect/1, disconnect/0, disconnect/1]).
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
    {ok, noreply}.

disconnect() ->
    disconnect(?Client).

disconnect(Name) ->
    Name ! {disconnect, self()},
    {ok, noreply}.

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
	{connect, _From} ->
	    ?Server ! {connect, self()},
	    loop(State);
	{disconnect, _From} ->
	    ?Server ! {disconnect, self()},
	    loop(State);
	{sum_areas, Shapes, From} ->
	    ?Server ! {sum_areas, Shapes, From},
	    loop(State);
	{reply, ?Server, {ok, Topic, Info}} ->
	    io:format("Client '~p' ~p. Successfully ~p.~n", [self(), Topic, Info]),
	    loop(State);
	{reply, ?Server, {ok, disconnected}} ->
	    io:format("Client '~p' is disconnected.~n", [self()]),
	    io:format("Client's process is exiting...~n", []),
	    exit(disconnect);
	{reply, ?Server, {error, Topic, Info}} ->
	    io:format("Client '~p' cannot ~p for ~p~n.", [self(), Topic, Info]),
	    io:format("Client process is terminating...~n", []),
	    exit(Info);
	Reply ->
	    io:format("Client '~p' recieved '~p'.~n", [self(), Reply]),
	    io:format("client's process is exiting...~n", []),
	    exit(exhausted)
    end.
