-module(prime).
-export([prime/1, list_primes_upto/1]).

prime(N) 
  when N < 2 ->
    false;
prime(2) ->
    true;
prime(N) 
  when N > 2 ->
    prime(N, 2, math:sqrt(N)).

prime(_, Ubound, Ubound) ->
    true;
prime(_, Lbound, Ubound)
  when Lbound > Ubound ->
    true;
prime(N, Lbound, _) 
  when N rem Lbound =:= 0 ->
    false;
prime(N, Lbound, Ubound)
  when N rem Lbound /= 0 ->
    prime(N, Lbound +1, Ubound).

list_primes_upto(N) ->
    list_primes_upto(N, []).

list_primes_upto(N, FirstPrime)
  when N < 2 ->
    {firstPrime, _} = {firstPrime, FirstPrime};
list_primes_upto(Upto, Primes) 
  when Upto >= 2 ->
    list_primes_upto(2, Upto, Primes).
list_primes_upto(Upto, Upto, Primes) ->
    Primes;
list_primes_upto(Lbound, Upto, Primes) ->
    case prime(Lbound) of 
	 true ->
	    io:format("~p~n" ,[Upto]),
	    list_primes_upto(Lbound + 1, Upto, Primes ++ [Lbound]);
	false ->
	    list_primes_upto(Lbound + 1, Upto, Primes)
        end. 
