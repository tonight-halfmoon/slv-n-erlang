-module(errh).
-export([start/0, request/1]).
-export([loop/0]).

-define(LProc, 'Add_one').

start() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, loop, []),
    register(?LProc, Pid),
    {ok, Pid}.

request(Int) ->
    ?LProc ! {request, self(), Int},
    receive
	{result, Result} ->
	    Result;
	{'EXIT', _Pid, Reason} ->
	    {error, Reason}
    after 1000 ->
	     timeout
    end.

loop() ->
    receive
	{request, Pid, Msg} ->
	    Pid ! {result, Msg + 1}
    end,
    loop().
