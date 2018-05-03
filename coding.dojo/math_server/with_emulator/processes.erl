
5> Pid = spawn(fun() -> ok end).
<0.69.0>
6> is_process_alive(Pid).
false
7> Pid = spawn(fun() -> receive M -> M end end). 
** exception error: no match of right hand side value <0.72.0>
8> Pid2 = spawn(fun() -> receive M -> M end end).
<0.75.0>
9> is_process_alive(Pid2).
true
10> is_process_alive(Pid2).
true
11> is_process_alive(Pid2).
true
12> Pid2 ! anything.
anything
13> is_process_alive(Pid2).
false
14> 
4> Pid2 = spawn(fun() -> receive M -> io:format("Emplyee to print: ~p.~n", [M]) end end).
<0.69.0>
5> Pid2 ! {please, printme, "Ahmad @ Valtech"}.                                          
Emplyee to print: {please,printme,"Ahmad @ Valtech"}.
{please,printme,"Ahmad @ Valtech"}
6> 
2> catch 1/0.
{'EXIT',{badarith,[{erlang,'/',[1,0],[]},
                   {erl_eval,do_apply,6,[{file,"erl_eval.erl"},{line,674}]},
                   {erl_eval,expr,5,[{file,"erl_eval.erl"},{line,431}]},
                   {shell,exprs,7,[{file,"shell.erl"},{line,687}]},
                   {shell,eval_exprs,7,[{file,"shell.erl"},{line,642}]},
                   {shell,eval_loop,3,[{file,"shell.erl"},{line,627}]}]}}
3> 1/0.
** exception error: an error occurred when evaluating an arithmetic expression
     in operator  '/'/2
        called as 1 / 0
4> 

