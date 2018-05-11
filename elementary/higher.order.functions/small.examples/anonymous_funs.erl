-module(anonymous_funs).
-export(export_all).


%%Anonymous functions, or funs, address that problem by letting you declare a special kind of function inline, without naming them. They can do pretty much everything normal functions can do, except calling themselves recursively (how could they do it if they anonymous?) Their syntax is: 

fun(Args1) ->
	Expression1,
	Expression2,
	...,
	    ExpN;
   (Args2) ->
	Expressssion1,
	Exp2,
	...,
	    ExpN;
end

%% in erl mode/ environment to define an anonymous function simply types the following 
1> Fn = fun() -> a end.
%and the environment /compiler will return something like this
#Fun<erl_eval.20.90072148>


%look how to define an anonymous function that prints out the passed argument. 

14> Fun2 = fun(X) -> io:format("This is your aargument's value: ~s.~n", [X]) end.
#Fun<erl_eval.6.90072148>
15> Fun2('DS').                                                                 
This is your variable's value: DS.
ok
16> 


%%Closures
%To understand closures, one must first understand scope. A functions's scope can be imagined as the place where all the variables and their vaues are stored. Take a look at the following anonymous function:

22> Fun4 = fun(A) -> B = A + 1, F = fun() -> A * B end, F() end.             
#Fun<erl_eval.6.90072148>
23> Fun4(4).
20
24> 

		 
%In the function fun(A) -> B = A + 1., A and B are both defined to be part of fun/1's scope. This means that anywhere inside fun/1, you can refer to A and B and expect a value to be bound to them. 
B and A are still bound to fun/1's scope, so the function F can still access them.This is because F inherits fun/1;s scope. Like most kinds of real-life inheritance, the parents can't get what the children have:

24> Fun5 = fun(A) -> B = A + 1, F = fun() -> A * B end, Fun5() end.
* 1: variable 'Fun5' is unbound
25> Fun5 = fun(A) -> B = A + 1, F = fun() -> A * B end, B end.     
#Fun<erl_eval.6.90072148>
26> Fun5(4).
5
27> Fun5(5).
6
28> Fun5(6).
7
29> Fun5(7).
8
30> Fun6 = fun(A) -> B = A + 1, F = fun() -> C = A * B end, F()end.  
#Fun<erl_eval.6.90072148>
31> Fun6(1).
2
32> Fun7 = fun(A) -> B = A + 1, F = fun() -> C = A * B end, F(), C end.
* 1: variable 'C' is unbound
33> a() -> Secret = "pony", fun() -> Secret end.
* 1: syntax error before: '->'
33> Sec = a() -> Secret = "pony", fun() -> Secret end.
* 1: syntax error before: '->'
33> Sec = a() -> Secret = "pony" end, fun() -> Secret end.
* 1: syntax error before: '->'
33> Sec = a() -> Secret = "pony",Secret end.              
* 1: syntax error before: '->'
33> Sec = fun() -> Secret = "pony",Secret end.
#Fun<erl_eval.20.90072148>
34> Sec.
#Fun<erl_eval.20.90072148>
35> Sec().
"pony"
36> Pass = fun(F) -> "Sec/0's password is " ++ F().
* 1: syntax error before: '.'
36> Pass = fun(F) -> "Sec/0's password is " ++ F() end.
#Fun<erl_eval.6.90072148>
37> Pass(Sec()).
** exception error: bad function "pony"
38> 
