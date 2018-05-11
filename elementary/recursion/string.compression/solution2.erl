-module(solution2).
-include_lib("eunit/include/eunit.hrl").
-export([main/0, compress/1]).

compress_empty_test() ->
    ?assertEqual([], compress([])).

compress_sol_test() ->
    ?assertEqual(a, compress([a])).

compress_two_test() ->
    ?assertEqual(a2, compress([a,a])).

compress_a3_test() ->
    ?assertEqual(a3b, compress([a,a,a,b])).

compress_aaab1_test() ->
    ?assertEqual(a3b1, compress([a,a,a,b,1])).

%% What is the right / optmial implementation to make both unit test pass and main function fwrite properly work? 20th Aug 2017  
main() ->
    {ok, [Line]} = io:fread("", "~ts"),
    io:fwrite("Input: ~s~n", [Line]),
    io:fwrite("~ts~n", [compress(Line)]),
    true.

compress([]) ->
    [];
compress([H|T]) ->
    compress(H, T, 1, []).

compress(Curr, [Curr|T], Occ, MSG) ->
    compress(Curr, T, Occ+1, MSG);
compress(Curr, [Next|T], 1, MSG) ->   
    compress(Next, T, 1, [Curr|MSG]);
compress(Curr, [Next|T], Occ, MSG) ->
    compress(Next, T, 1, [integer_to_list(Occ), Curr|MSG]);
compress(Last, [], 1, MSG) ->
    reverse([Last|MSG]);
compress(Last, [], Occ, MSG)->
    reverse([integer_to_list(Occ), Last|MSG]).


reverse([])->
    [];
reverse([H|T]) ->
    reverse(T, [H]).
reverse([H|T], L)->
    reverse(T, [H|L]);
reverse([], L) ->
    L.

