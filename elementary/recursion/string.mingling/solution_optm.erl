-module(solution_optm).
-include_lib("eunit/include/eunit.hrl").
-export([main/0]).

str_mngl_test() ->
    ?assertEqual([a,q,b,t,e,r], str_mngl([a,b,e], [q,t,r])).

str_mngl_2_test() ->
    ?assertEqual([a,q,b,t,e,r,m,p,n,y,l,u], str_mngl([a,b,e,m,n,l], [q,t,r,p,y,u])).

str_mngl_1_test() ->
    ?assertEqual([a,q,b,s], str_mngl([a,b], [q,s])).

str_mngl_3_test() ->
    ?assertEqual([a,q,b,t,e,r,m,p,n,y,l,u,v], str_mngl([a,b,e,m,n,l], [q,t,r,p,y,u,v])).

str_mngl_4_test() ->
    ?assertEqual([a,q,b,t,e,r,m,p,n,y,l,i], str_mngl([a,b,e,m,n,l,i], [q,t,r,p,y])).

str_mngl_5_test() ->
    ?assertEqual([q,t,r,p,y], str_mngl([], [q,t,r,p,y])).

str_mngl_empty_test() ->
    ?assertEqual([], str_mngl([], [])).

str_mngl_sol_and_empty_test() ->
    ?assertEqual([a], str_mngl([a], [])).

str_mngl_empty_and_sol_test() ->
    ?assertEqual([a], str_mngl([], [a])).

str_mngl_empty_and_twin_test() ->
    ?assertEqual([a, b], str_mngl([], [a, b])).

main() ->
    {P,Q} = read_2lines(),
    io:fwrite("~s", [str_mngl(P,Q)]).

str_mngl(P, Q) -> 
    str_mngl(P, Q, []).

str_mngl([], [], R) ->  
    reverse(R);
str_mngl([], Sol, R) ->
    lists:append(reverse(R), Sol);
str_mngl(Sol, [], R) ->
    lists:append(reverse(R), Sol);
str_mngl([HP|TP], [HQ|TQ], R) -> 
    str_mngl(TP, TQ, [HQ, HP|R]).

reverse(R) -> 
    reverse(R, []).

reverse([], R) -> 
    R;
reverse([H|T], R) -> 
    reverse(T, [H|R]).

read_2lines() ->
    {ok, [P]} = io:fread("", "~s"),
    {ok, [Q]} = io:fread("", "~s"),
    {P,Q}.
