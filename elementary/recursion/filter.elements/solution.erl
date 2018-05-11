-module(solution).
-export([main/0]).
-include_lib("eunit/include/eunit.hrl").

filter_elms_test() ->
    ?assertEqual([4,5,3], filter_elms([4,5,2,5,4,3,1,3,4], 2)).

main() ->
    {ok, [T]} = io:fread("", "~d"),
    Tstcs = consume_tstcs(T),
    process(Tstcs),
    true.

filter_elms(L,K) ->
    filter_elms(L, K, L, []).

filter_elms([], _, _, []) ->
    [-1];
filter_elms([], _, _, F) ->
    reverse(F);
filter_elms([H|T], K, R, F) ->
    case filter(H, K, 0, R) of 
	[] ->
	    filter_elms(T, K, R, F);
	Elm ->
	    Taken = takewhile(Elm, R), 
	    
	    filter_elms(T, K, Taken, [Elm|F])
    end.
 
filter(Elm, K, K, _)->
    Elm;
filter(_, _, _, []) ->
    [];
filter(Elm, K, Acc, [Elm|T]) ->
    filter(Elm, K, Acc+1, T);
filter(Elm, K, Acc, [_|T]) ->
    filter(Elm, K, Acc, T).

takewhile(Elm, L)->
    takewhile(Elm, L, []).

takewhile(Elm, [Elm|T], F)-> 
    takewhile(Elm, T, F);
takewhile(Elm, [H|T], F) ->
    takewhile(Elm, T, [H|F]);
takewhile(_, [], F) ->
    F.

reverse(L) -> reverse(L, []).
reverse([H|T], R) -> reverse(T, [H|R]);
reverse([], R) -> R. 

consume_tstcs(T) ->
    consume_tstcs(T, 0, []).
consume_tstcs(T, T, Tstcs) ->
    Tstcs;
consume_tstcs(T, I, Tstcs) ->
    {ok, [_,K]} = io:fread("", "~d~d"),
    List = read_int_list(),
    consume_tstcs(T, I+1, lists:append([ Tstcs, [{K, List}]])).

process([]) ->
    ok;
process([{K,L}|T]) ->
    %io:fwrite("~p~n", [filter_elms(L, K)]),
    lists:foreach(fun(X) -> io:format("~w ", [X]) end, filter_elms(L, K)),
    io:fwrite("~n"),
    process(T).    

read_int_list() ->
    case io:get_line("") of
	eof ->
	    ok;
	Line ->
	    lists:map(fun erlang:list_to_integer/1, string:tokens(Line, "\r\n\t "))
    end.
