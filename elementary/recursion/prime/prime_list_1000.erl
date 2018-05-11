-module(prime_list_1000).
-export([primes1000/0, p_primes1000/0]).
-import(prime2,[is_prime/1]).
-include_lib("eunit/include/eunit.hrl").

p_primes1000 () ->
    S1000 = lists:seq(2, 1000),
    lists:filter(fun(X) -> is_prime(X) end, S1000).

run_test()->
    Expected_value = [2, 3, 5, 7, 11, 13, 17, 19, 
		      23, 29, 31, 37, 41, 43, 47],
    ?assertEqual(primes1000(),Expected_value).

primes1000() ->
    foreach(2, 50, 2, ets:new(primes, [bag])).
foreach(_, End, End, Prime_set) ->
    [X || {prime,X} <- ets:tab2list(Prime_set)];
    %Prime_set;
foreach(2, 50, N, Prime_set) ->
    N2 = N + 1,
    Is_prime = is_prime(N),
    case Is_prime of 
	true ->
	    ets:insert(Prime_set,{prime,N})
		;
	false ->
	   ok
    end,	
    foreach(2, 50, N2, Prime_set).
