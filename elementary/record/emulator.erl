Erlang/OTP 19 [erts-8.3.5.5] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V8.3.5.5  (abort with ^G)
1> c(person).
{ok,person}
2> person:ahmad().
{person,"Ahmad",undefined,undefined}
3> person:new("P", 23, "23-34").
{person,"P",23,"23-34"}
4> #person.
* 1: syntax error before: '.'
4> #person.name.
* 1: record person undefined
5> P = person:ahmad().
{person,"Ahmad",undefined,undefined}
6> P#person.name.
* 1: record person undefined
7> P.name.
* 1: syntax error before: '.'
7> P.
{person,"Ahmad",undefined,undefined}
8> rr(person).
[person]
9> person:ahmad().
#person{name = "Ahmad",age = undefined,phone = undefined}
10> #person.
* 1: syntax error before: '.'
10> #person.name.
2
11> rd(newreco, {asd}).
newreco
12> rl().
-record(newreco,{asd}).
-record(person,{name,age,phone}).
ok
13> P = person:ahmad().
#person{name = "Ahmad",age = undefined,phone = undefined}
14> P#person.name.
"Ahmad"
15> 
