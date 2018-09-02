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
    call(Name, connect).

disconnect() ->
    disconnect(?Client).

disconnect(Name) ->
    case is_defined(Name) of
	true ->
	    call(Name, disconnect);
	_ ->
	    {error, client_not_defined}
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
    sum_areas(?Client, Shapes).

sum_areas(Name, Shapes) ->
    call(Name, {sum_areas, Shapes}).

async_sum_areas(Shapes) ->
    Ref = make_ref(),
    TrueClient = self(),
    spawn(fun() -> ?Client ! {{sum_areas, Shapes}, Ref, TrueClient} end),
    {ok, noreply}.

init(InitialState) ->
    loop(InitialState).

loop(State) ->
    receive
	{stop, From} ->
	    io:format("Client received ~p from ~p.~nStopping...~n", [stop, From]),
	    From ! {ok, self(), stopped};
	{connect, Ref, From} ->
	    ?Server ! {connect, {Ref, From}, self()},
	    loop(not_yet);
	{disconnect, Ref, From} ->
	    ?Server ! {disconnect, {Ref, From}, self()},
	    loop(State);
	{{sum_areas, Shapes}, Ref, From} ->
	    ?Server ! {sum_areas, Shapes, Ref, From},
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

call(ClientName, Protocol) ->
    Ref = make_ref(),
    ClientName ! {Protocol, Ref, self()},
    receive
	{reply, Ref, Reply} ->
	    Reply
    after 300 ->
	    exit(timeout)
    end.

is_defined(Name) ->
    case whereis(Name) of
	undefined ->
	    false;
	Pid when is_pid(Pid) ->
	    true
    end.
