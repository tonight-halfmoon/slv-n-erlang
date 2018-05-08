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
    stop(whereis(?math_server), ?math_server).

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

stop(undefined, _Name) ->
    {error, already_stopped};
stop(Pid, Name) ->
    case is_process_alive(Pid) of
	true ->
	    send_stop_protocol(Name),
	    {ok, stopped};
	false ->
	    {error, already_stopped}
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
	    Result = eval(F, Shapes),
	    Client ! {reply, {sum_areas, Result}},
	    loop(F);
	{request, From, {connect_client, Client}} ->
	    link(Client),
	    From ! {reply, {connect_client, {ok, client_connected, Client}}},
	    loop(F)
    after 40000 ->
	    io:format("Timeout. Server shutdown.~n", []),
	    exit(timeout)
    end.

eval(Fun, Args) ->
    case catch Fun(Args) of
	{'EXIT', Why} ->
	    {error, Why};
	Result ->
	    {ok, Result}
    end.
