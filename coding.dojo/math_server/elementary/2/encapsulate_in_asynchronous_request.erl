3> spawn(fun()-> receive after 1400 -> ok end, io:format("~p~n", [server:sum_areas([{square, 3}], Pid)]) end ). 
<0.68.0>
4> 
{ok,9}
4> q().
ok

