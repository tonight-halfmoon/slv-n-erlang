Compile and enter Erlang Emulator on the BEAM

$ erl -make $ erl -pa ebin/

1> {ok, Pida} = errh:start(0). {ok,<0.62.0>} 2> {ok, Pidb} = errh:start({0, Pida}). {ok,<0.64.0>}

--> Then, to experience the output, asynchronous reply from the serving process, spawn an anynomous function as follows:

3> spawn(fun() -> errh:increment({0, Pida}), receive Reply -> io:format('Reply is: ~p~n', [Reply]) end end).

N: 0 <0.77.0> Reply is: {reply,<0.62.0>,4}

