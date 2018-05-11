-module(remove_x).
-export([rm_x/2]).
-include_lib("eunit/include/eunit.hrl").
% Elm must not be list or atom

run_unit_test() ->
    ?assertEqual([a,c], rm_x([a,b,c],b)),
    ?assertEqual([a,b,c], rm_x([a,b,c,d],d)),
    ?assertEqual([], rm_x([],a)),
    ?assertEqual([a,b,c], rm_x([a,b,c],1)),
    ?assertEqual([b,c], rm_x([a,b,c],a)),
    ?assertEqual([a,b,c], rm_x([a,b,c],[a,b])).

rm_x([], _) ->
    [];
rm_x(Input, Elm) -> 
    rm_x(Input, Elm,[] , []).
rm_x([], Elm, Elm, [Elm|T2]) ->
    rm_x([], Elm, Elm, T2);
rm_x([], _, _, Output) ->
   lists:reverse(Output);
rm_x([H|T], Elm, Elm, [_|T2]) ->
    rm_x([H|T], Elm , H,  T2);
rm_x([H|T], Elm, _, Output) ->
    rm_x(T, Elm, H,  [H] ++ Output).


