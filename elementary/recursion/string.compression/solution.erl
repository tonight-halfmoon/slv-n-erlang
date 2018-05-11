%%% Jeddah SA; 11:05 p.m Munich Time
%%% compress method fails with Test Case #9 and # 14 at Hackerrank
%%% lists:append takes long time (at least more than 12 s, instead
%%% check solution2.erl

-module(solution).
-export([main/0]).

main() ->
    {ok, [Line]} = io:fread("", "~s"),
    io:fwrite("~s~n", [compress(Line)]),
    true.

compress([]) ->
    [];
compress([H|T]) ->
    compress(H, T, 1, []).
compress(Curr, [Curr|T], Occ, MSG) ->
    compress(Curr, T, Occ+1, MSG);
compress(Curr, [Next|T], 1, MSG) ->
    compress(Next, T, 1, lists:append([MSG, [Curr]]));
compress(Curr, [Next|T], Occ, MSG) ->
    compress(Next, T, 1, lists:append([MSG, [Curr], integer_to_list(Occ)]));
compress(Last, [], 1, MSG)->
   lists:append([MSG, [Last]]);
compress(Last, [], Occ, MSG)->
   lists:append([MSG, [Last], integer_to_list(Occ)]).
