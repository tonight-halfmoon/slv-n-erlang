-module(client).
-export([start/0, stop/0]).
-export([sum_areas/1, async_sum_areas/1]).
-export([init/1]).
-include("server.hrl").
-include("client.hrl").

start() ->
    Pid = spawn(?MODULE, init, [self()]),
    register(?Client, Pid),
    {ok, Pid}.

stop() ->
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
    monitor(process, whereis(?Server)),
    loop(InitialState).

loop(TrueClient) ->
    receive
       {sum_areas, Shapes, From} ->
           ?Server ! {sum_areas, Shapes, From},
	   loop(From);
       {'DOWN', _MonitorRef, _Type, _Object, Info} ->
	   io:format("Server down for ~p~n", [Info]),
	   exit(server_down);
       {disconnect, _From} ->
	   exit(disconnect);
	M ->
	   io:format("Client recevied unknown message: ~p~n", [M]),
	   loop(TrueClient)
    end.	
