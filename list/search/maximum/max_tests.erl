-module(max_tests).
-export([profile/3, testcase01/0]).
-include_lib("eunit/include/eunit.hrl").
-include("./include/testcase.01.hrl").

maximum__1_test_() ->
    {"max of '[1,2,3,5,9,0,12,8]' is '12'", ?_assertEqual(12, max:maximum([1,2,3,5,9,0,12,8]))}.

maxdc_tail2__1_test_() ->
    {"max of '[1,2,3,5,9,0,12,8]' is '12'", ?_assertEqual(12, max:maxdc_tail2([1,2,3,5,9,0,12,8]))}.

maxdc2__1_test_() ->
    {"max of '[1,2,3,5,9,0,12,8]' is '12'", ?_assertEqual(12, max:maxdc2([1,2,3,5,9,0,12,8]))}.

maxdc_tail_1_test_() ->
    {"max of '[1,2,3,5,9,0,12,8]' is '12'", ?_assertEqual(12, max:maxdc_tail([1,2,3,5,9,0,12,8]))}.

maxdc_1_test_() ->
    {"max of '[1,2,3,5,9,0,12,8]' is '12'", ?_assertEqual(12, max:maxdc([1,2,3,5,9,0,12,8]))}.

max__2_test_() ->
    {"max of '[100000000000, 999999090901, 1,2,3,5,9,0,12,8]' is '12'", ?_assertEqual(999999090901, max:maxdc_tail2([100000000000, 999999090901, 1,2,3,5,9,0,12,8]))}.

max__3_test_() ->
    {"max of '[10,1,2,3,5,9,0,12,8]' is '12'", ?_assertEqual(12, max:maxdc_tail2([10,2,3,5,9,0,12,8]))}.

max__empty_test_() ->
    {"max of '[]' is 'List is empty'", ?_assertEqual('List is empty', max:maxdc_tail2([]))}.

max_testcase_01_test_() ->
    {"max in 'testcase.01' is '999972'", ?_assertEqual(999972, max:maxdc_tail2(?testcase_01()))}.

testcase01() ->
    ?testcase_01().

profile(M, F, Params) ->
    io:fwrite("Length of Input List Params: ~w~n", [length(Params)]),
    {Micros, Foutput} = timer:tc(M, F, [Params]), 
    io:fwrite("Execution Time: ~w microseconds~n", [Micros]), 
    io:fwrite("Function Output: ~w~n", [Foutput]).

%%% 50> max:profile(max, maximum, max:testcase01()).
%%% Length of Input List Params: 100000
%%% Execution Time: 3029 microseconds
%%% Function Output: 999972
%%% ok
%%% 51> max:profile(max, maximum, max:testcase01()).
%%% Length of Input List Params: 100000
%%% Execution Time: 2834 microseconds
%%% Function Output: 999972
%%% ok
%%% 52> max:profile(max, maxdc_tail2, max:testcase01()).
%%% Length of Input List Params: 100000
%%% Execution Time: 36027 microseconds
%%% Function Output: 999972
%%% ok
%%% 53> max:profile(max, maxdc_tail, max:testcase01()). 
%%% Length of Input List Params: 100000
%%% Execution Time: 27310 microseconds
%%% Function Output: 999972
%%% ok
%%% 54> max:profile(max, maxdc, max:testcase01()).      
%%% Length of Input List Params: 100000
%%% Execution Time: 6462 microseconds
%%% Function Output: 999972
%%% ok
%%% 55> 
