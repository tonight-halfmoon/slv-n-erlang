-module(solution).
-export([main/0, rotate/1]).
-include_lib("eunit/include/eunit.hrl").

rotate_test() ->
    ?assertEqual("bcacababc", rotate([97,98,99])).

rotate2_test() ->
    ?assertEqual("bcacababc", rotate("abcd")).

rotate_singl_test() ->
    ?assertEqual("z", rotate([122])).

main() ->
    {ok, [T]} = io:fread("", "~d"),
    main(T),
    true.

main(T) ->
    main(T, 0, []).

main(0,_, _)->
    ok;
main(T,T, L)->
    process(L);
main(T, Nex, L)->
    {ok, [Str]} = io:fread("", "~s"),
    main(T, Nex+1, L ++ [Str]).

rotate(L)->
    rotate(L, length(L), 0, []).

rotate(_, 0, _, RTT) ->
    RTT;
rotate([X], _, _, _) ->
    io:fwrite("~c", [X]),
    [X];
rotate(_, Len, Len, RTT) ->
    RTT;
rotate([], _, _, RTT) ->
    RTT;
rotate([H|T], Len, I, RTT) ->
    Rith = T ++ [H],
    lists:foreach(fun(X) -> io:format("~c", [X]) end, Rith),
    io:fwrite(" "),
    rotate(Rith, Len, I+1, RTT ++ Rith).

process(ok) -> ok;
process([])->
    ok;
process([H|T])->
    rotate(H),
    io:fwrite("~n"),
    process(T).
