-module(p).
-export([start/0]).

start() ->
    spawn(fun snp/0).

snp() ->
   % process_flag(trap_exit, true),
    loop().

loop() ->
    receive
	{'EXIT', _Pid, normal} ->
	    io:format("Received 'EXIT' but normal ~n", []),
	    loop();
	{'EXIT', _Pid, Why} ->
	    io:format("Recived EXIT from; Why: ~p~n", [Why]),
	    loop();
	Msg ->
	    io:format("Received Msg: ~p~n", [Msg]),
	    loop()
    end.
