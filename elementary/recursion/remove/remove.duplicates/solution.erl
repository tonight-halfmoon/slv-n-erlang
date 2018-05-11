%% Jeddah, SA
-module(solution).
-export([main/0, rm_dups/1]).
-include_lib("eunit/include/eunit.hrl").

remove_dups_test() ->
    ?assertEqual([a,b,c], rm_dups([a,a,b,c,c,a])).

main() ->
    Line = io:get_line(""),
    io:fwrite("~s", [rm_dups(Line)]),
    true.

rm_dups(L)->
    rm_dups(L, []).

rm_dups([H|T], Nodups) ->
    case exists(H, Nodups) of
	true ->
	    rm_dups(T, Nodups);
	false ->
	    rm_dups(T, lists:append([Nodups, [H]]))
    end;
rm_dups([], Nodups) ->
    Nodups.

exists(X,[X|_])->
    true;
exists(X,[_|T]) ->
    exists(X,T);
exists(_,[]) ->
    false.
