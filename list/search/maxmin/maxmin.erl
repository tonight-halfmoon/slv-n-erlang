-module(maxmin).
-export([maxmin/1, testcase_01/0]).
-include_lib("eunit/include/eunit.hrl").
-include("./include/testcase.01.hrl").

%% simple implementation in tail recursion.
maxmin([]) ->
    'List is empty';
maxmin([H|T]) ->
    maxmin(T, H, H).

maxmin([], Mx, Mn) ->
    {{max, Mx}, {min, Mn}};
maxmin([H|T], Mx, Mn) ->
    maxmin(T, max(H, Mx), min(H, Mn)).

testcase_01() ->
    ?testcase_01().

max_testcase_01_test_() ->
    {"max in 'testcase.01' is '999972'", ?_assertEqual({{max,999972},{min,1}}, maxmin(?testcase_01()))}.
