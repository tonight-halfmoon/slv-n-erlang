-module(split2_profile_tests).
-include_lib("eunit/include/eunit.hrl").
-include("./include/testcase.01.hrl").
-export([profile/4, testcase01/0]).

profile_test_() ->
    {"It must execute function 'split' on 'testcase01' and successfully return with ok.", ?_assertEqual(ok, profile(split2, split, testcase01(), []))}.

testcase01() ->
    ?testcase_01().

profile(M, F, Params, Options) ->
    io:fwrite("Length of Input List Params: ~w~n", [length(Params)]),
    {Micros, Foutput} = timer:tc(M, F, [Params]), 
    io:fwrite("Execution Time: ~w microseconds~n", [Micros]), 
    case Options of 
	[ print_fun_output ] ->
	    io:fwrite("Function Output: ~w~n", [Foutput]);
	_ ->
	    ok
    end.
