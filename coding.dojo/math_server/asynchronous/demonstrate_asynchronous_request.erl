4> spawn(fun()-> server:sum_areas([{square, 3}], Pid),receive after 1500 -> ok end, receive M -> io:format("~p~n", [M]) end end ).
<0.74.0>
9  

