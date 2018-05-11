-module(owner_process).
-export([start/0, create_process/1]).

start() ->
    spawn(fun snp/0).

snp() ->
    process_flag(trap_exit, true),
    loop().

loop() ->
    receive
	{'EXIT', _Pid, normal} ->
	    io:format("Owner Received 'EXIT' but normal ~n", []),
	    loop();
	{'EXIT', _Pid, Why} ->
	    io:format("Owner Recived EXIT from; Why: ~p~n", [Why]),
	    loop();
	{cp, Fun} ->
	    Pid = spawn_link(Fun),
	    io:format("New child process created: ~p~n", [Pid]),
	    loop();
	Msg ->
	    io:format("Owner Received Msg: ~p~n", [Msg]),
	    loop()
	end.

create_process(Fun) ->
    spawn_link(Fun).

