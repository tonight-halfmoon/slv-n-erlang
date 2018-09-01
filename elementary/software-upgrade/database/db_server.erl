-module(db_server).
-export([start/0, stop/0, upgrade/1]).
-export([write/2, read/1, delete/1]).
-export([init/0, loop/1]).
-vsn(1.0).

-define(DBServer, 'db_server_proc').

start() ->
    register(?DBServer, spawn(?MODULE, init, [])).

stop() ->
    ?DBServer ! stop.

upgrade(Data) ->
    ?DBServer ! {upgrade, Data}.

write(Key, Data) ->
    ?DBServer ! {write, Key, Data}.

read(Key) ->
    ?DBServer ! {read, self(), Key},
    receive Reply -> Reply end.

delete(Key) ->
    ?DBServer ! {delete, Key}.

init() ->
    loop(db:new()).

loop(Db) ->
    receive
	{write, Key, Data} ->
	    loop(db:write(Key, Data, Db));
	{read, Pid, Key} ->
	    Pid ! db:read(Key, Db),
	    loop(Db);
	{delete, Key} ->
	    loop(db:delete(Key, Db));
	{upgrade, Data} ->
	    NewDb = db:convert(Data, Db),
	    ?MODULE:loop(NewDb);
	stop ->
	    db:destroy(Db)
    end.
