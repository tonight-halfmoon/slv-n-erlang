-module(solution).
-export([main/0]).
-include_lib("eunit/include/eunit.hrl").

str_mngl_test() ->
    ?assertEqual([a,q,b,t,e,r], str_mngl([a,b,e], [q,t,r])).
str_mngl_2_test() ->
    ?assertEqual([a,q,b,t,e,r,m,p,n,y,l,u], str_mngl([a,b,e,m,n,l], [q,t,r,p,y,u])).
str_mngl_1_test() ->
    ?assertEqual([a,q,b,s], str_mngl([a,b], [q,s])).
str_mngl_empty_test() ->
    ?assertEqual([], str_mngl([], [])).

main() ->
    {P,Q} = read_2lines(),
    io:fwrite("~s", [str_mngl(P,Q)]),
    % the following code takes longer time
    %lists:map(fun(X) -> io:format("~c", [X]) end, str_mngl(P,Q)),
    io:fwrite("~n"),
    true.

str_mngl(P,Q) ->
    str_mngl(P,Q,[]).
str_mngl([],[],R) -> 
    %lists:reverse(R);
    reverse(R);
str_mngl([HP1,HP2,HP3|TP], [HQ1,HQ2,HQ3|TQ], R) ->
    str_mngl(TP,TQ,  [HQ3,HP3,HQ2,HP2,HQ1,HP1|R]);
str_mngl([HP|TP], [HQ|TQ], R) ->
    str_mngl(TP,TQ, [HQ,HP|R]). % always add to the head!

read_2lines() ->
    {ok, [P]} = io:fread("", "~s"),
    {ok, [Q]} = io:fread("", "~s"),
    {P,Q}.

reverse(R)-> reverse(R, []).
reverse([], R) -> R;
reverse([H1,H2,H3|T], R) ->
    reverse(T, [H3,H2,H1|R]);
reverse([H|T], R) ->
    reverse(T, [H|R]).
