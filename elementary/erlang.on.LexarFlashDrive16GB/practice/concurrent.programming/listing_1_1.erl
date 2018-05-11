-module('listing_1_1').
-export([run/0]).

run() ->
    Pid = spawn(fun ping/0),
    Pid ! {ff,1, self()},
    receive 
	{Y , pong} ->
	    {ok, "  ", Y } 
    end.

ping() ->
    receive
	{ff, X, From} ->
	    Y = X +1,
	    From ! {Y , pong}
    end.

%% So what makes all of the above tick?
