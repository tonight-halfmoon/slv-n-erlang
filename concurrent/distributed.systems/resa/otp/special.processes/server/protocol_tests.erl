-module(protocol_tests).
-export([alloc/0, freeup/0, stats/0, stop_handler/0, stop_stats/0]).
-include("../config/config.hrl").
-include("config_internal.hrl").
-include("../config/telecommunication.hrl").

alloc() ->
    io:format("~p ~p: sending cask2alloc message to server~n", [?MODULE, self()]),
    io:format("~p; server pid: ~p~n", [?MODULE, whereis(?server)]),
    ?server ! #cask2alloc{client_pid=self()},
receive
    Msg ->
	io:format("~p received reply ~p~n", [?MODULE, Msg])
    end,
    true.

freeup() ->
    io:format("~p ~p: sending cask2free message to server~n", [?MODULE, self()]),
    io:format("~p; server pid: ~p~n", [?MODULE, whereis(?server)]),
    ?server ! #cask2free{client_pid=self(), resource=term_to_binary('ab.12')},
    receive
	Msg ->
	    io:format("~p received reply ~p~n", [?MODULE, Msg]) 
	end,
    true.

stats() ->
    io:format("~p ~p: sending cask4stats message to server~n", [?MODULE, self()]),
    io:format("~p; server pid: ~p~n", [?MODULE, whereis(?server)]),
    ?server ! #cask4stats{client_pid=self()},
    receive
	Msg ->
	    io:format("~p received reply ~p~n", [?MODULE, Msg]) 
	end,
    true.

stop_handler() ->
    io:format("~p ~p: sending stop 'handler' protocol ~n", [?MODULE, self()]),
    ?handler ! {stop, protocol_tests, self()},
    true.

stop_stats() ->
    io:format("~p ~p: sending stop 'stats provider' protocol ~n", [?MODULE, self()]),
    ?stats ! {stop, protocol_tests, self()}.

