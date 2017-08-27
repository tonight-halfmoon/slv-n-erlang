-module(plndrm).
-include_lib("eunit/include/eunit.hrl").
-export([check/1, slice_all_to_atom/1, slice_all/1]).

check_A_Toyota_Race_fast_safe_car_a_Toyota_test()->
    ?assertEqual(true, check("A Toyota! Race fast... safe car: a Toyota")).

check_Go_hang_a_salami_I_m_a_lasagna_hog_test()->
    ?assertEqual(true, check("Go hang a salami; I'm a lasagna hog!")).

check_Never_odd_or_even_palindrome_test() ->
    ?assertEqual(true, check("Never odd or even")).

check_Some_men_interpret_nine_memos_palindrome_test() ->
    ?assertEqual(true, check("Some men interpret nine memos.")).

chec_this_test()->
    ?assertEqual(true, check("Was it Eliot's toilet I saw?")).

check_not_palindrome_test() ->
    ?assertEqual(false, check("aabcsnasd")).

is_palindrome_test() ->
    ?assertEqual(true, check("asasa")).

is_palindrome_massachusitsstisuhcassam_test() ->
    ?assertEqual(true, check("massachusitsstisuhcassam")).

is_palindrome_empty_string_test() ->
    ?assertEqual(false, check("")).

is_palindrome_empty_test() ->
    ?assertEqual(false, check([])).

slice_all_test() ->
    ?assertEqual([a,b,c], slice_all_to_atom("abc")).

check([]) ->
    false;
check(S) ->
     string:equal(
       lists:concat(lists:filtermap(fun keep_pred/1, slice_all(S))),
       reverse(lists:concat(lists:filtermap(fun keep_pred/1, slice_all(S)))), true, none).

keep_pred("'") ->
    false;
keep_pred(" ") ->
    false;
keep_pred("?") ->
    false;
keep_pred(".") ->
    false;
keep_pred("!") ->
    false;
keep_pred(":") ->
    false;
keep_pred(",") ->
    false;
keep_pred(_) ->
    true.


slice_all([]) ->
    [];
slice_all(S) ->
    slice_all(S, []).

slice_all([], Slcs) ->
    reverse(Slcs);
slice_all([H|T], Slcs) ->
    Slc = string:slice([H|T], 0, 1),
    slice_all(T, [Slc|Slcs]).

slice_all_to_atom(S)->
   lists:map(fun erlang:list_to_atom/1, slice_all(S)).
   
reverse(S)->
    reverse(S,[]).

reverse([], R) ->
    R;
reverse([H|T], R) ->
    reverse(T, [H|R]).

