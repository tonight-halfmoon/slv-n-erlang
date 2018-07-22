-module(client).
-export([sum_areas/1, async_sum_areas/1]).
-export([start/0]).
-export([connect/0, disconnect/0]).
-export([init/1]).
-include("server.hrl").
-include("client.hrl").

start() ->
    Pid = spawn(?MODULE, init, [self()]),
    register(?Client, Pid),
    {ok, Pid}.

connect() ->
    ?Client ! {connect, self()},
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
%   monitor(process, whereis(?Server)),
    loop(InitialState).

loop(TrueClient) ->
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
       M ->
	   		io:format("Client recevied unknown message: ~p~n", [M]),
	   		loop(TrueClient)
    end.

