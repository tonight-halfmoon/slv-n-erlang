-module(n_swaps).
-export([nswp/2, swap_2500_profile/0]).
-include_lib("eunit/include/eunit.hrl").
    
nswp(L, K) ->
    S1 = swap(L),
    nswp(lists:nth(K,S1), K, 1, S1). 

nswp([], _, _, S) ->
    S;
nswp(L, K, _, _) when K > length(L) ->
    L;
nswp(_L, K, K, S) ->
    S;
%nswp([L|T], K, I, S) when is_list(L) ->
%    io:fwrite("Lists L: ~w, K: ~w, I: ~w, S: ~w~n", [L,K,I,S]),
%    nswp(T, K, I, concat(S, swap(L)));
nswp(L, K, I, S) ->
    io:fwrite("L: ~w, K: ~w, I: ~w, S: ~w~n", [L,K,I,S]),
    Swpith = swap(L),
    nswp(lists:nth(I,Swpith), K, I + 1, [Swpith|S]).

swap(L) ->
    swap(L, length(L), 1, []).

swap([], _, _, Slts) ->
    Slts;
swap(L, J, J, Slts) ->
    [L|Slts];
swap(L, M, J, Slts) ->
    ElemJ = lists:nth(J, L), 
    ElemM = lists:nth(M, L),
    SwpdJ = concat(ElemM, lists:delete(ElemM, lists:delete(ElemJ, L)), ElemJ),
    swap(L, M - 1, J, [SwpdJ|Slts]).

concat([], L) ->
    L;
concat([HoL|ToL], L) ->
    concat(ToL, [HoL|L]).

concat(L1, L2, L3) when is_list(L3) ->
    [L2|[L1|L3]];
concat(L1, L2, L3) when is_list(L1) ->
   [L2|[L3|L1]];
concat(L1, L2, L3) when is_list(L2) ->
    [L1|[L3|L2]].

swap_test_() ->
    {"Swapping '[1,2,3], 2' must result in '[1,2,3], [2,3,1] , [3,2,1]'", 
     %%?_assertEqual([[1,2,3], [2,3,1], [3,2,1]], swp([1,2,3], 1))}.
     ?_assertEqual([[1,2,3],[1,3,2],[3,2,1],[2,1,3],[2,3,1],[3,1,2]], nswp([1,2,3], 2))}.

swap_2500_test_() ->
    {"Swapping '[1..2500], 2500' must halt", 
    ?_assertMatch([L|_] when length(L) == 2500, nswp(lists:seq(1, 2500), 2500))}.

swap_2500_profile() ->
     {X, _O} = timer:tc(?MODULE, nswp, [lists:seq(1, 2500), 2500]), 
     io:fwrite("Profile: ~p microsends~n", [X]).

%% 31> n_swaps:swap_2500_profile().
%% profile: 477807ok
%% 32> n_swaps:swap_2500_profile().
%% profile: 255658ok
%% 33> c(n_swaps).                 
%% {ok,n_swaps}
%% 34> n_swaps:swap_2500_profile().
%% profile: 242197
%% ok
%% 35> c(n_swaps).                 
%% {ok,n_swaps}
%% 36> n_swaps:swap_2500_profile().
%% profile: 310692 microsends
%% ok
%% 37> c(n_swaps).                 
%% {ok,n_swaps}
%% 38> n_swaps:swap_2500_profile().
%% Profile: 275905 microsends
%% ok
%% 39> c(n_swaps).                 
%% {ok,n_swaps}
%% 40> n_swaps:swap_2500_profile().
%% Profile: 331789 microsends
%% ok
%% 41> n_swaps:swap_2500_profile().
%% Profile: 262128 microsends
%% ok
%% 42> n_swaps:swap_2500_profile().
%% Profile: 256386 microsends
%% ok
%% 43> n_swaps:swap_2500_profile().
%% Profile: 265713 microsends
%% ok
%% 44> n_swaps:swap_2500_profile().
%% Profile: 249074 microsends
%% ok
%% 45> n_swaps:swap_2500_profile().
%% Profile: 254008 microsends
%% ok
%% 46> n_swaps:swap_2500_profile().
%% Profile: 262497 microsends
%% ok
%% 47> 
