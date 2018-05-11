-module(factorial).
-export([start/1,fact/1]).

start(N) ->
   Pid = spawn(factorial, fact, [N]),
    Pid ! print_output.

fact(N) ->
    %io:format("~nFactorial(~w) = ~w.~n~n",[N,  fact(N, 1)])
    Output = fact(N, 1),
	receive 
	    print_output ->
		io:format("~nFactorial(~w) = ~w.~n~n",[N,  Output])
	end.


fact(0, Acc) ->
    Acc;
fact(N, Acc)
  when N >= 1 ->
    fact(N-1, N * Acc).
