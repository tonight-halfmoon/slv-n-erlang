2> rosemary@kali:~/erlang$ erl
Erlang/OTP 17 [erts-6.2] [source] [64-bit] [smp:8:8] [async-threads:10] [kernel-poll:false]

Eshell V6.2  (abort with ^G)
1> c(days).
{ok,days}
2> days: 
classify_day/1  module_info/0   module_info/1   
2> days:classify_day(saturday)/
2> .
* 2: syntax error before: '.'
2> days:classify_day(saturday).
{saturday," is a weekend day."}
3> lists:map(fun days:classify_day/1, [saturday, monday, 1234]).
[{saturday," is a weekend day."},
 {monday,"is a work day."},
 {1234," is not a day!"}]
4> 



53> lists:map(fun days:classify_day/1, [saturday, monday, tuesday, wednesday, 243 , sunday]). 
[{saturday," is a weekend day."},
 {monday,"is a work day."},
 {tuesday,"is a work day."},
 {wednesday,"is a work day."},
 {243," is not a day!"},
 {sunday," is a weekend day."}]



