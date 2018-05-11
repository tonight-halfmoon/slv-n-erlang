
%% Ref
%% http://erlang.org/pipermail/erlang-questions/2009-March/042531.html

%% Python
def gen(n):
        i = 0
        while i<n:
            yield i
            i+=1
        return

    x = gen(2)
    x.next() # returns 0
    x.next() # returns 1
    x.next()  # throws an exception
%%%


    t_fib(1) -> [1];
    t_fib(2) -> [1,1];
    t_fib(L) -> t_fib(3,L,[1,1]).

    t_fib(L,L,[N1,N2|_]=F) ->
        lists:reverse([N1+N2|F]);
    t_fib(C, L, [N1,N2|_]=F) ->
        t_fib(C+1,L,[N1+N2|F]).

   y_fib(L) ->
        y_fib(1, L, 1, 0).
    y_fib(L, L, N1 ,N2) ->
        ?last_yield(N1+N2);
    y_fib(C, L, N1, N2) ->
        N = N1+N2,
        ?yield(N),
        y_fib(C+1, L, N2, N).


    sum({M,Gen,Args}) ->
        Pid = spawn(M, Gen, Args),
        sum(Pid,0).
    sum(Pid,S) ->
        Pid ! {self(), next},
        receive
            {Pid, N} -> sum(Pid, S+N);
            {Pid, N, done} -> S+N
        end.

  > generators:sum({generators,y_fib,[100]}).  
    927372692193078999175

26> generators:batch_test(sum, 10000, 20).
    0.9438942747654998
    27> generators:batch_test(sum, 50000, 20).
    2.406425935803285
    28> generators:batch_test(fold, 10000, 20).
    0.6706634763086374
    29> generators:batch_test(fold, 50000, 20).
    1.3918125332818239
