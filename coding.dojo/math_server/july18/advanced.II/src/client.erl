-module(client).
-export([sum_areas/1, async_sum_areas/1]).
-export([connect/0, disconnect/0]).
-export([init/1]).
-include("server.hrl").
-include("client.hrl").

connect() ->
    Pid = spawn(?MODULE, init, [self()]),
    register(?Client, Pid),
    {ok, Pid}.

disconnect() ->
    ?Client ! {stop, self()},
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
       {diconnect, _From} ->
	   exit(stop);
	M ->
	   io:format("Client recevied unknown message: ~p~n", [M]),
	   loop(TrueClient)
    end.	

