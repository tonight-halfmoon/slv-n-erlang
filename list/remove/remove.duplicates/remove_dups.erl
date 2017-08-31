%% Jeddah, SA
-module(remove_dups).
-include_lib("eunit/include/eunit.hrl").
-export([main/0, rm_dups/1]).

remove_dups_test() ->
    ?assertEqual([a,b,c], rm_dups([a,a,b,c,c,a])).

remove_empty_lst_test() ->
    ?assertEqual([], rm_dups([])).

remove_no_dups_lst_test() ->
    ?assertEqual([a,b,v], rm_dups([a,b,v])).


remove_all_dups_lst_test() ->
    ?assertEqual([a], rm_dups([a,a,a])).

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

exists(_, []) ->
    false;
exists(X, [X|_])->
    true;
exists(X, [_|T]) ->
    exists(X, T).


main() ->
    Line = io:get_line(""),
    io:fwrite("~s", [rm_dups(Line)]),
    true.
