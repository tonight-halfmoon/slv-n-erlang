-module(digits).
-export([main/0, digits/1]).
-include_lib("eunit/include/eunit.hrl").

digits_neg0_test() -> 
    ?assertEqual([0], digits(-0)).

digits_neg4_test() -> 
    ?assertEqual([4], digits(-4)).

digits_neg_test() -> 
    ?assertEqual([2,9], digits(-29)).

digits_0sx0_test() -> 
    ?assertEqual([2,0], digits(00020)).

digits_test() ->
    ?assertEqual([1,2,3], digits(123)).

digits_8902393_test() ->
    ?assertEqual([8,9,0,2,3,9,3], digits(8902393)).

digits(N) when 0 > N ->
    digits(abs(N));
digits(N) when N >= 0 andalso N =< 9 ->
    [N];
digits(N) when not is_integer(N) ->
    <<"N must be an integer">>;
digits(N) ->
    digitize(N, []).

digitize(0, Ds)->
    Ds;
digitize(N, Ds)->
    digitize(trunc(N/10), [trunc(N rem 10)|Ds]).

main() ->
    {ok, [N]} = io:fread("", "~d"),
    Digits = digits(N),
    io:fwrite("~w", [Digits]),
    true.
