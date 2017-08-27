-module(replicate_elems).
-include_lib("eunit/include/eunit.hrl").
-export([main/0, repeat/2]).

replicate_elms_twice_test() ->
    ?assertEqual([1,1,1,2,2,2,3,3,3,4,4,4], repeat(2,[1,2,3,4])).

replicate_elms_twice_lng_test() ->
    ?assertEqual([1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,12,12,0,0,0,0,0,0], repeat(1, [1,2,3,4,5,6,7,8,9,10,11,12,12,0,0,0])).

replicate_empty_lst_test() ->
    ?assertEqual([], repeat(2,[])).

replicate_once_test() ->
    ?assertEqual([1,1,2,2], repeat(1,[1,2])).

replicate_3tms_test() ->
    ?assertEqual([1,1,1,1,2,2,2,2], repeat(3,[1,2])).

replicate_zero_times_test() ->
    ?assertEqual([1,2], repeat(0,[1,2])).

replicate_minusOne_times_test() ->
    ?assertEqual([], repeat(-1,[1,2])).

replicate_minus2_times_test() ->
    ?assertEqual("At least '-1' expected to replicate list elements", repeat(-2,[1,2])).

replicate_nested_1tms_test() ->
    ?assertEqual([1,1,[2,2,[[2,2,0,0,[0,0,[100,100]]]],9,9,0,0]], repeat(1,[1,[2,[[2,0,[0,[100]]]],9,0]])).

repeat(S, Xs) ->
    repeat(S, Xs, []).

repeat(S, _, _) when S < -1 ->
    "At least '-1' expected to replicate list elements";
repeat(0, Xs, _) -> 
    Xs;
repeat(_, [], L) -> 
    L;
repeat(S, [H|T], L) ->
    case is_list(H) of
	true ->
	    repeat(S, T, lists:append(L, [repeat(S, H, [])]));
	false ->
	    repeat(S, T, lists:append(L, repeatx(S, H)))
	end.

repeatx(S, X) ->
    repeatx(S, -1, X, []).

repeatx(0, _, _, Xs) ->
    Xs;
repeatx(S, S, _, Xs) ->
    Xs;
repeatx(S, I, X, Xs) ->
    repeatx(S, I+1, X, [X|Xs]).


main() ->
    {ok, [S]} = io:fread("", "~d"),
    Xs = read_list(),
    write_(repeat(S,Xs)),
    true.

read_list() ->
    read_list(true, []).

read_list(false, L) -> L;
read_list(true, L) ->
    Line = io:get_line(""),
    if Line =:= eof ->
	    read_list(false, L);
       true ->
            Tokens = string:tokens(string:strip(Line, right, $\n), "\n"),
            if Tokens =:= eof ->
                    read_list(false, L);
               Tokens =:= [] -> 
		    read_list(false, L);
               Tokens =:= ["eof"] ->
		    read_list(false, L);
               true ->
		    TokenInts = lists:map(fun erlang:list_to_integer/1, Tokens),
                     read_list(true, lists:append([L, TokenInts]))
	    end
    end.


write_([]) -> 
    ok;
write_(X) when not is_list(X) ->
    io:fwrite("~w~n", [X]);
write_(Xs) when is_list(Xs) ->
    [H|T] = Xs,
    write_(H),
    write_(T).
