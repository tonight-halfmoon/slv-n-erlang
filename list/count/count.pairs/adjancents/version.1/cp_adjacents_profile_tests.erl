-module(cp_adjacents_profile_tests).
-export([profile_cp_adj2/1, profile_cp_adj/1, tc_02_04/0]).
-include("include/testcase_input02_04.hrl").
-import(profile, [profile/3]).

tc_02_04() ->
    ?testcase_input02_04().

profile_cp_adj2(TestData) ->
    profile:profile(cp_adjacents, cp_adj2, TestData).


profile_cp_adj(TestData) ->
    profile:profile(cp_adjacents, cp_adj, TestData).


%% 4> cp_adjacents_profile_tests:profile_cp_adj2(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 19775 microseconds
%% ok
%% 5> cp_adjacents_profile_tests:profile_cp_adj(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 13819 microseconds
%% ok
%% 6> cp_adjacents_profile_tests:profile_cp_adj(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 16912 microseconds
%% ok
%% 7> cp_adjacents_profile_tests:profile_cp_adj2(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 15814 microseconds
%% ok
%% 8> cp_adjacents_profile_tests:profile_cp_adj(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 13658 microseconds
%% ok
%% 9> cp_adjacents_profile_tests:profile_cp_adj2(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 14070 microseconds
%% ok
%% 10> cp_adjacents_profile_tests:profile_cp_adj2(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 19828 microseconds
%% ok
%% 11> cp_adjacents_profile_tests:profile_cp_adj(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 13643 microseconds
%% ok
%% 12> cp_adjacents_profile_tests:profile_cp_adj2(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 15009 microseconds
%% ok
%% 13> cp_adjacents_profile_tests:profile_cp_adj(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 15091 microseconds
%% ok
%% 14> cp_adjacents_profile_tests:profile_cp_adj2(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 15280 microseconds
%% ok
%% 15> cp_adjacents_profile_tests:profile_cp_adj(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 16053 microseconds
%% ok
%% 16> cp_adjacents_profile_tests:profile_cp_adj2(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 19786 microseconds
%% ok
%% 17> cp_adjacents_profile_tests:profile_cp_adj(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 14901 microseconds
%% ok
%% 18> cp_adjacents_profile_tests:profile_cp_adj2(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 20902 microseconds
%% ok
%% 19> cp_adjacents_profile_tests:profile_cp_adj(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 15865 microseconds
%% ok
%% 20> cp_adjacents_profile_tests:profile_cp_adj2(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 19735 microseconds
%% ok
%% 21> cp_adjacents_profile_tests:profile_cp_adj2(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 13815 microseconds
%% ok
%% 22> cp_adjacents_profile_tests:profile_cp_adj2(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 19062 microseconds
%% ok
%% 23> cp_adjacents_profile_tests:profile_cp_adj2(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 16311 microseconds
%% ok
%% 24> cp_adjacents_profile_tests:profile_cp_adj2(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 15788 microseconds
%% ok
%% 25> cp_adjacents_profile_tests:profile_cp_adj2(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 15531 microseconds
%% ok
%% 26> cp_adjacents_profile_tests:profile_cp_adj2(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 14738 microseconds
%% ok
%% 27> cp_adjacents_profile_tests:profile_cp_adj2(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 16798 microseconds
%% ok
%% 28> cp_adjacents_profile_tests:profile_cp_adj2(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 14305 microseconds
%% ok
%% 29> cp_adjacents_profile_tests:profile_cp_adj2(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 16550 microseconds
%% ok
%% 30> cp_adjacents_profile_tests:profile_cp_adj2(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 16336 microseconds
%% ok
%% 31> cp_adjacents_profile_tests:profile_cp_adj2(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 14225 microseconds
%% ok
%% 32> cp_adjacents_profile_tests:profile_cp_adj2(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 15292 microseconds
%% ok
%% 33> cp_adjacents_profile_tests:profile_cp_adj2(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 14971 microseconds
%% ok
%% 34> cp_adjacents_profile_tests:profile_cp_adj2(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 16970 microseconds
%% ok
%% 35> cp_adjacents_profile_tests:profile_cp_adj2(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 15610 microseconds
%% ok
%% 36> cp_adjacents_profile_tests:profile_cp_adj2(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 16826 microseconds
%% ok
%% 37> cp_adjacents_profile_tests:profile_cp_adj2(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 15272 microseconds
%% ok
%% 38> cp_adjacents_profile_tests:profile_cp_adj2(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 18960 microseconds
%% ok
%% 39> cp_adjacents_profile_tests:profile_cp_adj2(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 17839 microseconds
%% ok
%% 40> cp_adjacents_profile_tests:profile_cp_adj2(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 12596 microseconds
%% ok
%% 41> cp_adjacents_profile_tests:profile_cp_adj2(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 16410 microseconds
%% ok
%% 42> cp_adjacents_profile_tests:profile_cp_adj2(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 15898 microseconds
%% ok
%% 43> cp_adjacents_profile_tests:profile_cp_adj2(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 15391 microseconds
%% ok
%% 44> cp_adjacents_profile_tests:profile_cp_adj2(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 18357 microseconds
%% ok
%% 45> cp_adjacents_profile_tests:profile_cp_adj2(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 15630 microseconds
%% ok
%% 46> cp_adjacents_profile_tests:profile_cp_adj2(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 17829 microseconds
%% ok
%% 47> cp_adjacents_profile_tests:profile_cp_adj2(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 14484 microseconds
%% ok
%% 48> cp_adjacents_profile_tests:profile_cp_adj2(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 14974 microseconds
%% ok
%% 49> cp_adjacents_profile_tests:profile_cp_adj2(cp_adjacents_profile_tests:tc_02_04()). 
%% Execution time: 20768 microseconds
%% ok
%% 50> 
%% 50> cp_adjacents_profile_tests:profile_cp_adj(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 14435 microseconds
%% ok
%% 51> cp_adjacents_profile_tests:profile_cp_adj(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 15514 microseconds
%% ok
%% 52> cp_adjacents_profile_tests:profile_cp_adj(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 15324 microseconds
%% ok
%% 53> cp_adjacents_profile_tests:profile_cp_adj(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 16885 microseconds
%% ok
%% 54> cp_adjacents_profile_tests:profile_cp_adj(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 15858 microseconds
%% ok
%% 55> cp_adjacents_profile_tests:profile_cp_adj(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 15554 microseconds
%% ok
%% 56> cp_adjacents_profile_tests:profile_cp_adj(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 13564 microseconds
%% ok
%% 57> cp_adjacents_profile_tests:profile_cp_adj(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 17963 microseconds
%% ok
%% 58> cp_adjacents_profile_tests:profile_cp_adj(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 13396 microseconds
%% ok
%% 59> cp_adjacents_profile_tests:profile_cp_adj(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 14996 microseconds
%% ok
%% 60> cp_adjacents_profile_tests:profile_cp_adj(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 14801 microseconds
%% ok
%% 61> cp_adjacents_profile_tests:profile_cp_adj(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 14535 microseconds
%% ok
%% 62> cp_adjacents_profile_tests:profile_cp_adj(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 15805 microseconds
%% ok
%% 63> cp_adjacents_profile_tests:profile_cp_adj(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 14389 microseconds
%% ok
%% 64> cp_adjacents_profile_tests:profile_cp_adj(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 14166 microseconds
%% ok
%% 65> cp_adjacents_profile_tests:profile_cp_adj(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 14614 microseconds
%% ok
%% 66> cp_adjacents_profile_tests:profile_cp_adj(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 15032 microseconds
%% ok
%% 67> cp_adjacents_profile_tests:profile_cp_adj(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 15435 microseconds
%% ok
%% 68> cp_adjacents_profile_tests:profile_cp_adj(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 9620 microseconds
%% ok
%% 69> cp_adjacents_profile_tests:profile_cp_adj(cp_adjacents_profile_tests:tc_02_04()).
%% Execution time: 13782 microseconds
%% ok
%% 70> 
