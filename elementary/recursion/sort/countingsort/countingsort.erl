-module(countingsort).
-include_lib("eunit/include/eunit.hrl").
-export([main/0]).

cnts_test() -> 
    ?assertEqual([1,3,4,6], cnts([6,4,1,3])).

cnts_lrg_lst_test() -> 
    ?assertEqual([1,4,5,6,7,9,10,11,12,14,52], cnts([5,6,4,10,11,12,1,14,9,52,7])).

cnts_empty_lst_test() -> 
    ?assertEqual([], cnts([])).

main() ->
    InputA = [6,3,5,1],
    Rk = prepareR(InputA),
    io:fwrite("Rk: ~w~n", [Rk]),
    %Sorted_output = cs(InputA, Rk, []),
    %io:fwrite("Sorted output is ~p~n", [Sorted_output]),
    true.

%cs([], _ , _) -> [];
%cs(InputA, Rk, Sorted_output) ->
%    c.

prepareR([])->[];
prepareR(InputA)->
    Max = max_(InputA),
    io:format("Max: ~p~n", [Max]),
    Rinitd = initR(InputA, Max),
    io:format("Rinitd: ~p~n", [Rinitd]),
    Rready = sumsubset1s(Rinitd),
    io:format("Rready: ~p~n", [Rready]),
    Rready.

sumsubset1s(L) ->
    Reversed = lists:reverse(L), 
    sumsubset1s(Reversed, []).
sumsubset1s([], L) ->
    L;
sumsubset1s([H|T], L) ->
    Sublist = lists:sublist([H|T], length([H|T]) - 1),
    case eq(H,1) of
	true->
	    sumsubset1s(T, [1+ sumones(Sublist)|L]);
	false ->
	    sumsubset1s(T, [0|L])
    end.

sumones(L) -> sumones(L, 0).
sumones([H|T], Acc) -> 
    case eq(H,1) of 
	true -> 
	    sumones(T, Acc+1);
	false ->
	    sumones(T, Acc)
    end; 
sumones([],Acc) -> Acc. 

initR(InputA, Max) ->
    initR(InputA,Max, 1,[]).

initR(_,Max,Max, R) -> R;
initR([H|T], Max, First, R) ->
    case eq(H,Max) of 
	true ->
	    initR(T, Max, First+1, [1|R]);
	false ->
	    initR(T, Max, First+1, [0|R])
    end.
max_([H|T])->
    max_(T, H).
max_([],Max)->
    Max;
max_([H|T], Max) ->
    case gr(H, Max) of
	true ->
	    max_(T, H);
	false ->
	    max_(T,Max)
    end.

gr(X,Y)->
    X>Y.
eq(X,Y)->
    X=:=Y.
