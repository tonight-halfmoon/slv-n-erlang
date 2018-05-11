-module(solution).
-export([main/0]).
-include_lib("eunit/include/eunit.hrl").

filter_test()->
    ?assertEqual([2,1,0], filter_(3, [10,9,8,2,7,5,1,3,0])).

main() ->
    {ok, [Delimiter]}  = io:fread("", "~d"),
    Xs = read_list(),
    write_(filter_(Delimiter, Xs)),
    true.

filter_(Dl, L) ->
    filter_(fun dlmtr/2, Dl, L).

filter_(_, _, []) -> 
    [];
filter_(Pred, Dl, [H|T]) -> 
    case Pred(H,Dl) of 
	true ->
	    [H| filter_(Pred, Dl,T)];
	false ->
	    filter_(Pred, Dl, T)
    end.

dlmtr(X,Dl) when X < Dl ->
    true;
dlmtr(X,Dl) when X >= Dl ->
    false.
    

read_list() ->
    read_list(true, []).
read_list(false, L) ->
    L;
read_list(true, L) ->
    Line = io:get_line(""),
    if Line =:= eof ->
	    read_list(false, L);
       true ->
	    Tokens = string:tokens(string:strip(Line,right,$\n) ,"\n"),
	    if Tokens =:= [] ->
		    read_list(false,L);
	       %Tokens =:= "eof" ->
		%    read_list(false, L);
	       true ->
		    TokenInts = lists:map(fun erlang:list_to_integer/1, Tokens),
		    read_list(true, lists:append([L, TokenInts]))
		end
    end.

write_([]) ->
    ok;
write_(L) when is_list(L) ->
    [H|T] = L,
    write_(H),
    write_(T);
write_(X) ->
    io:fwrite("~w~n", [X]).


    
