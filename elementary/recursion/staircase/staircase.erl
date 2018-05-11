-module(staircase).
-export([main/0]).
-include_lib("eunit/include/eunit.hrl").
drawstaircase_test() ->
    ?assertEqual(ok, drawstaircase(6)).

main() ->
    {ok,[N]} = io:fread("", "~d"),
    drawstaircase(N),
    true.

drawstaircase(N) -> drawstaircase(0, N).
drawstaircase(N, N) -> ok;
drawstaircase(Start, N) -> putspace(1, N - Start),
			   puthash(0, Start + 1),
			   io:format("~n"),
			   drawstaircase(Start + 1, N).

puthash(Limit, Limit) -> ok;
puthash(Howmany, N) -> io:format("~tc", [35]),
		       puthash(Howmany + 1, N).

putspace(Limit,Limit) -> ok;
putspace(Howmany, N )-> io:format(" "),
			putspace(Howmany, N  - 1).
