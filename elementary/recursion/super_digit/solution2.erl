-module(solution2).
-export([main/0, super_digit/1]).
-include_lib("eunit/include/eunit.hrl").

super_digit_1_test() -> ?assertEqual(2, super_digit(9875)).

super_digit_148_3_test() -> ?assertEqual(3, p_superd(148, 3)).

super_digit_testcase5_test() -> ?assertEqual(5, p_superd(3546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736,100000)).

main() ->
    {ok, [N,K]} = io:fread("", "~d~d"),
    io:fwrite("~p~n", [p_superd(N, K)]),
    true.

p_superd(N, K) -> 
    super_digit(p(N,K)).

super_digit(X) -> 
    super_digit(X, 0, 0).

super_digit(0, Rem, Super) ->
    Stent = Rem+Super, 
    io:fwrite("Stent: ~p~n", [Stent]),
    case solo_digit(Stent) of 
	false -> 
	    super_digit(Stent);
	true->
	    Stent
    end;
super_digit(X, Rem, Super)  ->
    super_digit(div10(X), rem10(X), Rem + Super).


p(N,K) -> 
    p(N, K-1, 0, num_digits(N), N).

p(_, K, K, _, P) -> 
    P;
p(N, K, I, Digits, Pcur) ->
    Pith = times10(Pcur, Digits) + N,
    p(N, K, I + 1, Digits, Pith).

rem10(X) -> 
    rem_(X, 10).

rem_(X,Y) -> 
    X rem Y.

div10(X) -> 
    div_(X, 10).

div_(_, 0) -> 
    undefined;
div_(_, 1) -> 
    1;
div_(X,Y) -> 
    trunc(X / Y).

num_digits(X) -> 
    num_digits(X, 0).

num_digits(0, Digits) -> 
    Digits;
num_digits(X, Digits) -> 
    num_digits(div10(X), Digits + 1).

solo_digit(X) when 0 =< X andalso X =< 9 -> 
    true;
solo_digit(_) -> 
    false.

times10(X, N) -> 
    times10(X, N, 0, X).

times10(_X, N, N, Res) -> 
    Res;
times10(X, N, I, Res) -> 
    times10(X, N, I+1, Res * 10). 


p_4_100k_test() -> ?assertEqual(861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788861568688536788, p(861568688536788, 100)).

p_5_100k_test() ->
    ?assertEqual(3546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736354663094731205145301417215964793598447882494597314133306225261371802568871670447054744972388662673635466309473120514530141721596479359844788249459731413330622526137180256887167044705474497238866267363546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736354663094731205145301417215964793598447882494597314133306225261371802568871670447054744972388662673635466309473120514530141721596479359844788249459731413330622526137180256887167044705474497238866267363546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736354663094731205145301417215964793598447882494597314133306225261371802568871670447054744972388662673635466309473120514530141721596479359844788249459731413330622526137180256887167044705474497238866267363546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736354663094731205145301417215964793598447882494597314133306225261371802568871670447054744972388662673635466309473120514530141721596479359844788249459731413330622526137180256887167044705474497238866267363546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736354663094731205145301417215964793598447882494597314133306225261371802568871670447054744972388662673635466309473120514530141721596479359844788249459731413330622526137180256887167044705474497238866267363546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736354663094731205145301417215964793598447882494597314133306225261371802568871670447054744972388662673635466309473120514530141721596479359844788249459731413330622526137180256887167044705474497238866267363546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736354663094731205145301417215964793598447882494597314133306225261371802568871670447054744972388662673635466309473120514530141721596479359844788249459731413330622526137180256887167044705474497238866267363546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736354663094731205145301417215964793598447882494597314133306225261371802568871670447054744972388662673635466309473120514530141721596479359844788249459731413330622526137180256887167044705474497238866267363546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736354663094731205145301417215964793598447882494597314133306225261371802568871670447054744972388662673635466309473120514530141721596479359844788249459731413330622526137180256887167044705474497238866267363546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736354663094731205145301417215964793598447882494597314133306225261371802568871670447054744972388662673635466309473120514530141721596479359844788249459731413330622526137180256887167044705474497238866267363546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736354663094731205145301417215964793598447882494597314133306225261371802568871670447054744972388662673635466309473120514530141721596479359844788249459731413330622526137180256887167044705474497238866267363546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736354663094731205145301417215964793598447882494597314133306225261371802568871670447054744972388662673635466309473120514530141721596479359844788249459731413330622526137180256887167044705474497238866267363546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736354663094731205145301417215964793598447882494597314133306225261371802568871670447054744972388662673635466309473120514530141721596479359844788249459731413330622526137180256887167044705474497238866267363546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736354663094731205145301417215964793598447882494597314133306225261371802568871670447054744972388662673635466309473120514530141721596479359844788249459731413330622526137180256887167044705474497238866267363546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736354663094731205145301417215964793598447882494597314133306225261371802568871670447054744972388662673635466309473120514530141721596479359844788249459731413330622526137180256887167044705474497238866267363546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736354663094731205145301417215964793598447882494597314133306225261371802568871670447054744972388662673635466309473120514530141721596479359844788249459731413330622526137180256887167044705474497238866267363546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736354663094731205145301417215964793598447882494597314133306225261371802568871670447054744972388662673635466309473120514530141721596479359844788249459731413330622526137180256887167044705474497238866267363546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736354663094731205145301417215964793598447882494597314133306225261371802568871670447054744972388662673635466309473120514530141721596479359844788249459731413330622526137180256887167044705474497238866267363546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736354663094731205145301417215964793598447882494597314133306225261371802568871670447054744972388662673635466309473120514530141721596479359844788249459731413330622526137180256887167044705474497238866267363546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736354663094731205145301417215964793598447882494597314133306225261371802568871670447054744972388662673635466309473120514530141721596479359844788249459731413330622526137180256887167044705474497238866267363546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736354663094731205145301417215964793598447882494597314133306225261371802568871670447054744972388662673635466309473120514530141721596479359844788249459731413330622526137180256887167044705474497238866267363546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736354663094731205145301417215964793598447882494597314133306225261371802568871670447054744972388662673635466309473120514530141721596479359844788249459731413330622526137180256887167044705474497238866267363546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736354663094731205145301417215964793598447882494597314133306225261371802568871670447054744972388662673635466309473120514530141721596479359844788249459731413330622526137180256887167044705474497238866267363546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736354663094731205145301417215964793598447882494597314133306225261371802568871670447054744972388662673635466309473120514530141721596479359844788249459731413330622526137180256887167044705474497238866267363546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736354663094731205145301417215964793598447882494597314133306225261371802568871670447054744972388662673635466309473120514530141721596479359844788249459731413330622526137180256887167044705474497238866267363546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736354663094731205145301417215964793598447882494597314133306225261371802568871670447054744972388662673635466309473120514530141721596479359844788249459731413330622526137180256887167044705474497238866267363546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736354663094731205145301417215964793598447882494597314133306225261371802568871670447054744972388662673635466309473120514530141721596479359844788249459731413330622526137180256887167044705474497238866267363546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736354663094731205145301417215964793598447882494597314133306225261371802568871670447054744972388662673635466309473120514530141721596479359844788249459731413330622526137180256887167044705474497238866267363546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736354663094731205145301417215964793598447882494597314133306225261371802568871670447054744972388662673635466309473120514530141721596479359844788249459731413330622526137180256887167044705474497238866267363546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736354663094731205145301417215964793598447882494597314133306225261371802568871670447054744972388662673635466309473120514530141721596479359844788249459731413330622526137180256887167044705474497238866267363546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736354663094731205145301417215964793598447882494597314133306225261371802568871670447054744972388662673635466309473120514530141721596479359844788249459731413330622526137180256887167044705474497238866267363546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736354663094731205145301417215964793598447882494597314133306225261371802568871670447054744972388662673635466309473120514530141721596479359844788249459731413330622526137180256887167044705474497238866267363546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736354663094731205145301417215964793598447882494597314133306225261371802568871670447054744972388662673635466309473120514530141721596479359844788249459731413330622526137180256887167044705474497238866267363546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736, p(3546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736, 100)).

super_didit_5_test() -> ?assertEqual(2, super_digit(3546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736)).

psuperd_test() -> ?assertEqual(3, p_superd(148, 3)).

psuperd_4_20k_test() -> ?assertEqual(6, p_superd(861568688536788, 20)).

psuperd_5_3k_test() -> ?assertEqual(4, p_superd(3546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736, 3)).


num_digits_test() -> ?assertEqual(4, num_digits(1345)).

num_digits_2_test() -> ?assertEqual(1, num_digits(1)).

p_test() -> ?assertEqual(123123123, p(123,3)).


times10_test() -> ?assertEqual(12300, times10(123,2)).

num_digits_4_test() -> ?assertEqual(15, num_digits(861568688536788)).

num_digits_5_test() -> ?assertEqual(100, num_digits(3546630947312051453014172159647935984478824945973141333062252613718025688716704470547449723886626736)).
