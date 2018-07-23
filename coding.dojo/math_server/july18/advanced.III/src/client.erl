-module(client).
-export([sum_areas/1, async_sum_areas/1]).
-export([start/0, start/1]).
-export([connect/0, connect/1, disconnect/0]).
-export([init/1]).
-include("server.hrl").
-include("client.hrl").

start() ->
    start(?Client).

start(Name) ->
    Pid = spawn(?MODULE, init, [self()]),
    register(Name, Pid),
    {ok, Pid}.

connect() ->
    connect(?Client).

connect(Name) ->
    Name ! {connect, self()},
    {ok, noreply}.

disconnect() ->
    ?Client ! {disconnect, self()},
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

loop(_State) ->
    receive
	{sum_areas, Shapes, From} ->
	    ?Server ! {sum_areas, Shapes, From},
	    loop(From);
	{'DOWN', _MonitorRef, _Type, _Object, Info} ->
	    io:format("Server down for ~p~n", [Info]),
	    exit(server_down);
	{connect, _From} ->
	    ?Server ! {connect, self()},
	    loop(self());
	{disconnect, _From} ->
	    ?Server ! {disconnect, self()},
	    loop(self());
	{reply, From, connected} ->
	    io:format("Client ~p is connected to server ~p~n", [self(), From]),
	    loop(self());
	{reply, _From, disconnected} ->
	    io:format("Client ~p is disconnected~n", [self()]),
	    exit(disconnected);
	Reply ->
	    io:format("~p~n", [Reply]),
	    io:format("client's process exiting...~n", []),
	    exit(exhausted)
    end.
