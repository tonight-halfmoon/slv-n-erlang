-module(solution).
-export([main/0]).
-include_lib("eunit/include/eunit.hrl").

odd_elems_sum_test() ->
    ?assertEqual(16, odd_sum([3,2,4,6,5,7,8,0,1])).

odd_elems_sum_empty_test() ->
    ?assertEqual(0, odd_sum([])).

odd_elems_sum_even_test() ->
    ?assertEqual(0, odd_sum([8,16,0])).

main() ->
    Xs = read_list(),
    write_(odd_sum(Xs)),
    true.

odd_sum(L)->
    sum_(fun odd/1, L, 0).
sum_(Pred, [H|T], Sum) -> case Pred(H) of true ->
    sum_(Pred, T, Sum+H);
			      false -> sum_(Pred, T, Sum) 
			  end;
sum_(_, [], Sum) ->
    Sum.

odd(X) when X rem 2 =/= 0 ->
    true;
odd(_) ->
    false.

read_list()->
    read_list(true, []).
read_list(true, L) ->
    Line = io:get_line(""),
    if Line =:= eof ->
	    read_list(false, L);
       true ->
	    Tokens = string:tokens(string:strip(Line,right,$\n),"\n"),
	    if Tokens =:= [] ->
		    read_list(false, L);
	       Tokens =:= ["eof"] ->
		    read_list(false, L);
	       true ->
		    TokenInts = lists:map(fun erlang:list_to_integer/1, Tokens),
		    read_list(true, lists:append([L, TokenInts])) 
	    end
    end;
read_list(false, L) ->
    L.

write_([]) ->
    ok;
write_(L) when is_list(L) ->
    [H|T] = L,
    write_(H),
    write_(T);
write_(X) ->
    io:fwrite("~w~n", [X]).
