% erl 
Erlang/OTP 19 [erts-8.3.5.5] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V8.3.5.5  (abort with ^G)
1> c(build_index).
{ok,build_index}
2> build_index:
index/1        module_info/0  module_info/1  
2> build_index:index("text.txt").
"collection"             3
"contains"               2
"deep"          4, 1
"line"           2
"safe"           1
"satoyot"                1
"second"                 2
"speed"          1
"think"          4
"this"          2, 1
"thoughts"               3
"toyota"                 1
"word"          3, 2
ok
3> q().
ok
4> 