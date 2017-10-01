-module(benchmarking).
-export([benchmark_adj/0, benchmark_adj2/0]).
-include("include/testcase_input02_04.hrl").
-import(cp_adjacents_profile_tests, [tc_02_04/0]).

-define(Limit, 10000).

benchmark_adj() ->
    benchmark(cp_adj, ?Limit, 0, []).

benchmark_adj2() ->
    benchmark(cp_adj, ?Limit, 0, []).

benchmark(Fun, 0, 0, R) ->
    {Fun, ?Limit, lists:sum(R)/length(R), 'microseconds in average'};
benchmark(Fun, C, B, R) ->
    benchmark(Fun, C-1, B, [profile(cp_adjacents, Fun, cp_adjacents_profile_tests:tc_02_04())|R]).

profile(Module, Fun, L) ->
    {Microseconds, _out} = timer:tc(Module, Fun, [L]),
    Microseconds.


%% 13> benchmarking:benchmark_adj().
%% {cp_adj,100,8381.43,'microseconds in average'}
%% 14> c(benchmarking).             
%% {ok,benchmarking}
%% 15> benchmarking:benchmark_adj2().
%% {cp_adj,10000,8873.3834,'microseconds in average'}
%% 16> benchmarking:benchmark_adj(). 
%% {cp_adj,10000,8806.539,'microseconds in average'}


%% 1000000
%% Erlang/OTP 20 [erts-9.0.1] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:10] [hipe] [kernel-poll:false]

%% Eshell V9.0.1  (abort with ^G)
%% 1> benchmarking:benchmark().
%% 7130.203656
%% 2> benchmarking:benchmark().
%% 7162.850537
