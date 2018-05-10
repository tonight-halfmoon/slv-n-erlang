
$ erl -make

$ erl -pa ebin/

Eshell V9.3  (abort with ^G)

1> {ok, Pid} = server:start().

{ok,<0.63.0>}

2> server:s

start/0      stop/1       sum_areas/2  

2> server:sum_areas([{circle, 3}], Pid).

{ok,28.274333882308138}

3> server:stop(Pid).

{ok,stopped}

4> q().

ok

5> 
