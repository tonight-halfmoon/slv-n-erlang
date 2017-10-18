-module(swap_count).
-export([swap_tail2/1, swap_2500_profile/0]).
-include_lib("eunit/include/eunit.hrl").

swap_tail2_i([], S) ->
    S;
swap_tail2_i([_], S) ->
     1 + S;
swap_tail2_i([_,_], S) ->
    2 + S;
swap_tail2_i([_,_,_], S) ->
    3 + S;
swap_tail2_i(L, S) ->
    swap_tail2(L, S).

swap_tail2(L) ->
    swap_tail2(L, 1).

swap_tail2([], _) ->
    0;
swap_tail2([_], S)->
    S;
swap_tail2([_, _], S) ->
    2 * S;
swap_tail2([_, _, _], S) ->
    3 * S;
swap_tail2(L, S) ->
    {Left, Right} = split(L),
    swap_tail2_i(Left, S) * swap_tail2_i(Right, S).
    
split([]) ->
    {[],[]};
split([X]) ->
    {[X],[]};
split([H|T]) -> 
    split(T, [H]).

split(L, R) when length(L) =:= length(R);
		 length(L) =:= length(R) + 1 ->
    {R, L};
split([H|T], R) ->
    split(T, [H|R]).

swap_2500_profile() ->
     {X, _O} = timer:tc(?MODULE, swap_tail2, [lists:seq(1, 2500)]), 
     io:fwrite("Profile: ~p microsends~n", [X]).

swap_test_one_test_() ->
    {"Swap [a] count '1'", ?_assertEqual(1, swap_tail2([a]))}.

swap_test_two_test_() ->
    {"Swap [a] count '2'", ?_assertEqual(2, swap_tail2([a,a]))}.

swap_tail2_test_() ->
    {"swapping '[1,2,3]' counts '3'", 
     ?_assertEqual(3, swap_tail2([1,2,3]))}.

swap_tail2_1234_test_() ->
    {"swapping '[1,2,3,4]' counts '9'", 
     ?_assertEqual(9, swap_tail2([1,2,3,4]))}.

swap_tail2_12345_test_() ->
    {"swapping '[1,2,3,4,5]' counts '12'", 
     ?_assertEqual(12, swap_tail2([1,2,3,4,5]))}.


swap_tail2_1234567_test_() ->
    {"swapping '[1,2,3,4,5,6,7]' counts '36'", 
     ?_assertEqual(36, swap_tail2([1,2,3,4,5,6,7]))}.

swap_empty_test_() ->
    {"swapping '[]' counts '0'", 
     ?_assertEqual(0, swap_tail2([]))}.

swap_tail2_8_elem_list_test_() ->
    {"swap [1,2,3,4,5,6,7,8] must halt and match certain expectation",
     ?_assertEqual(81, swap_tail2([1,2,3,4,5,6,7,8]))}.

swap_2500_test_() ->
    {"Swap '[1..2500]' must halt and count '?????????????'", 
    %?_assertMatch([[L|_]|_] when length(L) == 14520, swap_tail2(lists:seq(1, 2500)))}.
     ?_assertEqual(110783167907686255098431939266451380959811300330921613040977288118409055784212903490955438869635223772883929868683074765174437102918193503632596793373689122189864537122396739384473542431816408232288000705221147615008774920399949182950668621842848071709000502443797352637129698155656482589630668271838276663745349940508110251804273336465356246250505308850041347605424125083342765834123325946999774970273700567341155011542909314391312661606284098100475169582345697491080015015270525967684976428688225319763881275099530246260088888200670122927456256, swap_tail2(lists:seq(1, 2500)))}.
