-module(server).
-export([start/0, stop/0, sum_areas/2,
	 connect_client/1,
	 init/1]).

-include("server.hrl").

-define(NOTEST, true).
-include_lib("eunit/include/eunit.hrl").

start() ->
    start(?math_server).

stop() ->
    stop(?math_server).

sum_areas(Shapes, Client) ->
    cast({sum_areas, Shapes}, Client).

connect_client(Pid) ->
    call({connect_client, Pid}).

call({connect_client, Client}) ->
    ?math_server ! {request, self(), {connect_client, Client}},
    receive
	{reply, {connect_client, {ok, client_connected, Client}}} ->
	    {ok, client_connected, Client}
    after 50 ->
	    exit(timeout)
    end.

cast({sum_areas, Shapes}, Client) ->
    ?math_server ! {request, {sum_areas, Shapes}, Client},
    {ok, noreply}.

init(F) ->
    process_flag(trap_exit, true),
    loop(F).

start(Name) ->
    case whereis(Name) of
	undefined ->
	    Pid = spawn(?MODULE, init, [fun geometry:areas/1]),
	    register(Name, Pid),
	    {ok, Pid};
	Pid when is_pid(Pid) ->
	    {error, already_started}
    end.

stop(Name) ->
    stop(whereis(Name), Name).

stop(undefined, _Name) ->
    ok;
stop(Pid, Name) ->
    case is_process_alive(Pid) of
	true ->
	    send_stop_protocol(Name);
	false ->
	    ok
    end.

send_stop_protocol(Name) ->
    Name ! stop.

loop(F) ->
    receive
	{'EXIT', From, Why} ->
	    io:format("Server has received an Exit flag from ~p and will shutdown immediately. Reason: ~p~n", [From, Why]),
	    exit(Why);
	stop ->
            exit(shutdown);
	{request, {sum_areas, Shapes}, Client} ->
	    handle_sum_areas(F, Shapes, Client),
	    loop(F);
	{request, From, {connect_client, Client}} ->
	    handle_connect_client(From, Client),
	    loop(F)
    after 40000 ->
	    io:format("Timeout. Server shutdown.~n", []),
	    exit(timeout)
    end.

handle_sum_areas(F, Shapes, Client) ->
    case catch F(Shapes) of
	Areas when is_float(Areas); is_integer(Areas) ->
	    Client ! {reply, {sum_areas, ok, Areas}};
	Result ->
	    Client ! {reply, {sum_areas, error, Result}}
    end.

handle_connect_client(From, Client) ->
    link(Client),
    From ! {reply, {connect_client, {ok, client_connected, Client}}}.
