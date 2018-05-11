-module(analyse).
-export([analyse/1,listen/0]).

analyse([]) ->
    [];
analyse(L) ->
    io:format("L is~w~n ", [L]).

listen() ->
    receive
	{analyse, L} ->
	    analyse(L)
    end.

	    
