-module(solution).
-export([main/0]).
-include_lib("eunit/include/eunit.hrl").

replicate_list_elems_test() ->
    ?assertEqual([1,1,2,2,3,3,4,4], repeat(2,[1,2,3,4])).

replicate_list_elems2_test() ->
  ?assertEqual([1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10], repeat(2,[1,2,3,4,5,6,7,8,9,10])).

replicate_list_empty_test() ->
    ?assertEqual([], repeat(2,[])).

replicate_list_0_test() ->
    ?assertEqual([], repeat(1,[1,2])).

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

repeat(S, Xs) ->
    repeat(S, Xs, []).
repeat(_, [], L) -> L;
repeat(1, Xs, _) -> Xs;
repeat(S, [H|T], L) ->
    repeat(S, T, lists:append([L, repeatx(S, H)])).
repeatx(S, X) ->
    repeatx(S, 0, X, []).
repeatx(S, S, _, Xs)->
    Xs;
repeatx(0, _, _, Xs)->
    Xs;
repeatx(S, I, X, Xs) ->
    repeatx(S, I+1, X, [X|Xs]).

write_([]) -> ok;
write_(X) when not is_list(X) ->
    io:fwrite("~w~n", [X]);
write_(Xs) when is_list(Xs) ->
    [H|T] = Xs,
    write_(H),
    write_(T).
