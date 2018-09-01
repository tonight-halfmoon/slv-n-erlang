Erlang/OTP 19 [erts-8.3.5.5] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V8.3.5.5  (abort with ^G)
1> Bump = fun(Int) -> Int + 1 end.
#Fun<erl_eval.6.118419387>
2> (fun(Int) -> Int + 1 end)(2).
3
3> q().
3>

Erlang/OTP 19 [erts-8.3.5.5] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V8.3.5.5  (abort with ^G)
1> c(common_patterns_fun_expressions).
common_patterns_fun_expressions.erl:29: Warning: variable 'Xs' is unused
{ok,common_patterns_fun_expressions}
2> c(common_patterns_fun_expressions).
{ok,common_patterns_fun_expressions}
3> common_patterns_fun_expressions:evens([[2,3,4,5]]).
[[2,4]]
4> c(common_patterns_fun_expressions).
common_patterns_fun_expressions.erl:50: function foreach/1 undefined
error
5> c(common_patterns_fun_expressions).
{ok,common_patterns_fun_expressions}
6> common_patterns_fun_expressions:foreach(fun(X) -> io:format("Element: ~p~n", [X]) end, [2,3,4,5,6,10]).
Element: 2
Element: 3
Element: 4
Element: 5
Element: 6
Element: 10
ok
7> common_patterns_fun_expressions:foreach(fun(X) -> self() ! X end, [2,3,4,5,6,10]).
ok
8> flush().
Shell got 2
Shell got 3
Shell got 4
Shell got 5
Shell got 6
Shell got 10
ok
9> 
Erlang/OTP 19 [erts-8.3.5.5] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V8.3.5.5  (abort with ^G)
1> c(functions_as_results).
{ok,functions_as_results}
2> functions_as_results:times(3).
#Fun<functions_as_results.0.113893616>
3> functions_as_results:times(3)(2).
* 1: syntax error before: '('
3> (functions_as_results:times(3))(2).
6
4> Times = functions_as_results:times(3).
#Fun<functions_as_results.0.113893616>
5> Times(2).
6
6> functions_as_results:times(3).
#Fun<functions_as_results.0.113893616>
7> functions_as_results:times(3).
#Fun<functions_as_results.0.113893616>
8> c(functions_as_results).
functions_as_results.erl:17: function map/2 undefined
error
9> c(functions_as_results).
functions_as_results.erl:22: function map/1 undefined
error
10> c(functions_as_results).
{ok,functions_as_results}
11> functions_as_results:doubleAll([1,2,3,4,5]).
[2,4,6,8,10]
12> Pid = spawn(fun(X) -> receive {X, Pid} -> io:format("Element: ~p~n", [X]), Pid ! {self(), helloagain} end end ).
<0.85.0>
13> 
=ERROR REPORT==== 1-Sep-2018::20:51:15 ===
Error in process <0.85.0> with exit value:
{{badarity,{#Fun<erl_eval.6.118419387>,[]}},[{erlang,apply,2,[]}]}

13> Pid ! {asd, self()}/
13> Pid ! {asd, self()}.
** exception error: an error occurred when evaluating an arithmetic expression
     in operator  '/'/2
        called as {asd,<0.57.0>} / <0.85.0>
14> Pid ! {asd, self()}
14> .
{asd,<0.87.0>}
15> Pid = spawn(fun() -> receive {m, Pid} -> io:format("received m from: ~p~n", [Pid]), Pid ! {hello, self()} end end).
** exception error: no match of right hand side value <0.90.0>
16> f().
ok
17> Pid = spawn(fun() -> receive {m, Pid} -> io:format("received m from: ~p~n", [Pid]), Pid ! {hello, self()} end end).
<0.94.0>
18> Pid ! {m , self()}.
received m from: <0.91.0>
{m,<0.91.0>}
19> Pid ! {m , self()}.
{m,<0.91.0>}
20> c(shadowed_variables).
shadowed_variables.erl:7: Warning: variable 'X' is unused
shadowed_variables.erl:8: Warning: variable 'X' shadowed in 'fun'
{ok,shadowed_variables}
21> shadowed_variables:foo().
11
22> shadowed_variables:bar().
13
23> 
17> Pid = spawn(fun() -> receive {m, Pid} -> io:format("received m from: ~p~n", [Pid]), Pid ! {hello, self()} end end).
<0.94.0>
18> Pid ! {m , self()}.
received m from: <0.91.0>
{m,<0.91.0>}
19> Pid ! {m , self()}.
{m,<0.91.0>}
20> Pid = spawn(fun() -> receive {m, Pid} -> io:format("received m from: ~p~n", [Pid]), Pid ! {hello, self()} end; () receive after 1000 -> ok  end end).
* 1: syntax error before: 'receive'
20> c(shadowed_variables).
shadowed_variables.erl:7: Warning: variable 'X' is unused
shadowed_variables.erl:8: Warning: variable 'X' shadowed in 'fun'
{ok,shadowed_variables}
21> shadowed_variables:foo().
11
22> shadowed_variables:bar().
13
23> Positive = fun(X) when X > 0 -> true;(X) when X< = 0 -> false end.
* 1: syntax error before: '='
23> a.
a
24> Positive = fun(X) when X > 0 -> true; (X) when X < 0 -> false end.
#Fun<erl_eval.6.118419387>
25> Positive(X).
* 1: variable 'X' is unbound
26> Positive(3).
true
27> 
27> Positive(0).
** exception error: no function clause matching erl_eval:'-inside-an-interpreted-fun-'(0) 
28> Positive(4).
true
29> Positive(-1).
false
30> c(positive).
{ok,positive}
31> positive:positive([1,2,3,4,-7,98,3,0,-43,-12,34,5]).
[1,2,3,4,98,3,0,34,5]
32> c(positive).
{ok,positive}
33> c(positive).
{ok,positive}
34> positive:positive([12,23,-1,-432]).
[12,23]
35> rd(positive).
** exception error: undefined shell command rd/1
36> rr(positive).
[]
37> ?Positive.
* 1: syntax error before: '?'
37> Positive= fun(X) when X >=0 -> true; (X) when X < 0 -> false end.
** exception error: no match of right hand side value #Fun<erl_eval.6.118419387>
38> f().
ok
39> c(functions_as_results).
{ok,functions_as_results}
40> Positive = fun(X) when X >=0 -> true; (X) when X < 0 -> false end.
#Fun<erl_eval.6.118419387>
41> lists:all(Positive, [1,2,3,43,-453,-23,0,12]).
false
42> lists:all(Positive, [1,2,2,3,4,5,65,743,34,34]).
true
43> lists:any(Positive, [1,23,-2]).
true
44> lists:any(Positive, [-12]).
false
45> Sum = fun(E, Acc) -> E + Acc end.
#Fun<erl_eval.12.118419387>
46> lists:foldl(Sum , 0, [1,2,3,4,5,6,7]).
28
47> 
