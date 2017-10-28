-module(perms_tests).
-include_lib("eunit/include/eunit.hrl").

perms_test_() ->
    {
      "It must generate the corrent permutations", 
      ?_assertEqual([[1,2,3],[1,3,2],[2,3,1],[2,1,3],[3,2,1],[3,1,2]], perms:permute([1,2,3]))
    }.

perms_12345_test_() ->
    {
      "It must generate the corrent permutations", 
      ?_assertMatch(L when length(L) == 120, perms:permute([1,2,3,4,5]))
    }.

%permute_2500_test_() ->
%    {"Permutat '[1..2500]' must halt", 
%    ?_assertMatch(L when length(L) == 2500, perms:permute(lists:seq(1, 2500)))}.

permute_9_test_() ->
    {"Permutat '[1..9]' must halt", 
    ?_assertMatch(L when length(L) == 362880, perms:permute(lists:seq(1,9)))}.

%permute_11_test_() ->
%    {"Permutat '[1..11]' must halt", 
%    ?_assertMatch(L when length(L) == 11, perms:permute(lists:seq(1, 11)))}.


%permute_35_test_() ->
%    {"Permutat '[1..35]' must halt", 
%    ?_assertMatch(L when length(L) == 35, perms:permute(lists:seq(1, 35)))}.
