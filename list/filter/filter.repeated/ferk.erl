-module(ferk).
-export([main/0]).
-include_lib("eunit/include/eunit.hrl").

%%% Filter elements of a list which repeated at least k times.

ferk(L, K) ->
    ferk(L, K, L, []).

ferk([], _, _, []) ->
    [];
ferk([], _, _, Fltrd) ->
    reverse(Fltrd);
ferk([H|T], K, R, F) ->
    case filter(H, K, 0, R) of 
	[] ->
	    ferk(T, K, R, F);
	Elm ->
	    Taken = takeout(Elm, R),    
	    ferk(T, K, Taken, [Elm|F])
    end.
 
filter(Elm, K, K, _)->
    Elm;
filter(_, _, _, []) ->
    [];
filter(Elm, K, Acc, [Elm|T]) ->
    filter(Elm, K, Acc+1, T);
filter(Elm, K, Acc, [_|T]) ->
    filter(Elm, K, Acc, T).

takeout(Elm, L)->
    takeout(Elm, L, []).

takeout(Elm, [Elm|T], F)-> 
    takeout(Elm, T, F);
takeout(Elm, [H|T], F) ->
    takeout(Elm, T, [H|F]);
takeout(_, [], F) ->
    F.

reverse(L) -> reverse(L, []).
reverse([H|T], R) -> reverse(T, [H|R]);
reverse([], R) -> R. 


ferk_2times_test() ->
    ?assertEqual([4,5,3], ferk([4,5,2,5,4,3,1,3,4], 2)).

ferk_2elms_2times_test() ->
    ?assertEqual([], ferk([2,0,0,3,0,1,2,3,4], 23)).

main() ->
    {ok, [T]} = io:fread("", "~d"),
    Tstcs = consume_tstcs(T),
    process(Tstcs),
    true.

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
    %io:fwrite("~p~n", [ferk(L, K)]),
    lists:foreach(fun(X) -> io:format("~w ", [X]) end, ferk(L, K)),
    io:fwrite("~n"),
    process(T).    

read_int_list() ->
    case io:get_line("") of
	eof ->
	    ok;
	Line ->
	    lists:map(fun erlang:list_to_integer/1, string:tokens(Line, "\r\n\t "))
    end.
