-module(remove_x2).
-include_lib("eunit/include/eunit.hrl").
-export([rm/2, rm_t/2]).

run_unit_on_rm_t_test() ->
    ?assertEqual([a,c], rm_t(b, [a,b,c])),
    ?assertEqual([a,b,c], rm_t(d, [a,b,c,d])),
    ?assertEqual([], rm_t(a, [])),
    ?assertEqual([a,b,c], rm_t(1, [a,b,c])),
    ?assertEqual([b,c], rm_t(a, [a,b,c])),
    ?assertEqual([c], rm_t([a,b],[[a,b],c])).


run_unit_in_rm_test() ->
    ?assertEqual([a,c], rm(b, [a,b,c])),
    ?assertEqual([a,b,c], rm(d, [a,b,c,d])),
    ?assertEqual([], rm(a, [])),
    ?assertEqual([a,b,c], rm(1, [a,b,c])),
    ?assertEqual([b,c], rm(a, [a,b,c])),
    ?assertEqual([c], rm([a,b],[[a,b],c])).

rm(_, []) -> [];
rm(X, [X|T]) -> T;
rm(X, [Y|T]) -> [Y|rm(X, T)].

rm_t(X, List) -> rm_t(X, List, []).  
rm_t(_, [], Out) -> Out;
rm_t(X, [X|T], Out) -> rm_t(X, T, Out);
rm_t(X, [Y|T], Out) -> rm_t(X, T, lists:append(Out, [Y])).
