-module(perms_profile_tests).
-export([profile/1]).

profile(L) ->
    {Microseconds, _PermutationsList} = timer:tc(perms, permute, [L]),
    io:fwrite("Execution time: ~w microseconds~n", [Microseconds]).


%%% Profile data on T530
%%% 2> perms_profile_tests:profile([1,2,3,4,5,6]).
%%% Execution time: 4098 microseconds
%%% ok
%%% 3> perms_profile_tests:profile([1,2,3,4,5,6,7,8,9]).
%%% Execution time: 315572 microseconds
%%% ok
%%% 4> perms_profile_tests:profile([1,2,3,4,5,6,7,8,9,10]).
%%% Execution time: 2794671 microseconds
%%% ok
%%% 5> perms_profile_tests:profile([1,2,3,4,5,6,7,8,9,10,11]).
%%% Execution time: 28969776 microseconds
%%% ok
%%% 6> perms_profile_tests:profile([1,2,3,4,5,6,7,8,9,10,11,12]).
%%% eheap_alloc: Cannot allocate 29247236448 bytes of memory (of type "heap").

%%% Crash dump is being written to: erl_crash.dump...done
%%% rosemary@SCUBA:~/programming/erlang/slv-n-erlang/list/permutations$ erl
%%% Erlang/OTP 20 [erts-9.0.1] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:10] [hipe] [kernel-poll:false]

%%% Eshell V9.0.1  (abort with ^G)
%%% 1> perms_profile_tests:profile([1,2,3,4,5,6,7,8,9,10]).
%%% Execution time: 5627135 microseconds
%%% ok
%%% 2> perms_profile_tests:profile([1,2,3,4,5,6,7,8,9,10,11]).
%%% Execution time: 76128908 microseconds
%%% ok
%%% 3> perms_profile_tests:profile([1,2,3,4,5,6,7,8,9,10]).   
%%% Execution time: 2845803 microseconds
%%% ok
%%% 4> perms_profile_tests:profile([1,2,3,4,5,6,7,8,9,10,11]).
%%% Execution time: 54160711 microseconds
%%% ok
%%% 5> perms_profile_tests:profile([1,2,3,4,5,6,7,8,9,10,11]).
%%% Execution time: 42892287 microseconds
%%% ok
