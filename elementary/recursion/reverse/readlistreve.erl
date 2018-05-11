-module(readlistreve).
-export([main/0]).
-include_lib("eunit/include/eunit.hrl").

reverse_one_test()->
    ?assertEqual([a], reverse([a])).

reverse_two_test()->
    ?assertEqual([b,a], reverse([a,b])).

reverse_three_test()->
    ?assertEqual([c,b,a], reverse([a,b,c])).

reverse_many_test()->
    ?assertEqual([i,h,g,f,e,d,c,b,a], reverse([a,b,c,d,e,f,g,h,i])).

reverse_many2_test()->
    ?assertEqual([h,g,f,e,d,c,b,a], reverse([a,b,c,d,e,f,g,h])).

reverse_many3_test()->
    ?assertEqual([1,2,4,1,60,80,90,5,0], reverse([0,5,90,80,60,1,4,2,1])).

%% buggy : if user entered alpha characters main will crash!
main() ->
    %L = [19, 22, 3, 28, 26, 17,18, 4, 28, 0],
    Data = io:get_line(""),
    io:format("~s~n", [Data]),
    L =[1],% lists:map(fun erlang:list_to_integer/1, string:tokens(string:strip(Data, right, $\n), " ")),
    %io:format("~s~n", [L]),
    
    case check_constraints(L) of 
	true->
	    print((reverse(L)));
	false ->
	    unexpected_list
    end.

reverse([H|T]) -> reverse(T, [H]).
reverse([], L) -> L;
reverse([H|T], L) -> reverse(T, [H|L]).


print([H|T])->
    io:format("~p~n", [H]),
    print(T);
print([]) ->
    ok.

check_constraints(L)->
    case grte(length(L),1) andalso lse(length(L), 100) of
	true->
	    check_elem_constraints(L);
	false ->
	    false
    end.

check_elem_constraints([H|T]) ->
    case grte(H,0) andalso lse(H,100) of
		true ->
		    check_elem_constraints(T);
		false ->
		     false
	    end;
check_elem_constraints([]) ->
    true.

grte(X,Y)->
    X>=Y.
lse(X,Y)->
   X=<Y.
