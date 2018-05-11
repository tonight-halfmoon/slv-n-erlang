-module(list_comprehension).
-export([lc/1]).
-export([even/1]).
-export([zip/2]).
-export([qs_lc/1]).
-export([eqless/1]).
-export([gr/1]).
-export([reverse/1]).
-export([max/1]).


% {x e R : x = x^2}
% The set notation tells you the results you want will be all real numbers who are equal to their own square.
% List comprehensions in Erlang are about building sets from other sets.
% {2n : n in L}
lc([]) ->
    [];
lc(Set) -> 
    [2*N || N <- Set].

% brackets ({}) become square brackets ([]), the colon (:) becomes two pipes (||) and the word 'in' becomes the arrow (<-). 

even([]) ->
    [];
even(Set) ->
    [X || X <- Set, X rem 2 =:= 0].

zip(Set1, Set2) ->
    [{Op1,Op2} || Op1 <- Set1, Op2 <- Set2].
  

%qs_lc([]) ->
%    [];
%qs_lc([Pivot|Rest]) -> [{Qle} || {[Pivot|Qle]} <- Rest, Pivot =< Rest] ++ [Pivot] ++ [{Lrg} || {[Pivot|Lrg]} <- Rest, Pivot > Rest].

eqless([Pivot|Rest]) ->
    [Qle || Qle <- Rest, Qle =< Pivot].

gr([Pivot|Rest]) ->
    [Gr || Gr <- Rest, Gr > Pivot].

%qs_lc([Pivot|Rest]) -> eqless([Pivot|Rest]) ++ [Pivot] ++ gr([Pivot|Rest]).

qs_lc([])-> [];
qs_lc([Pivot|Rest]) ->
    qs_lc([Qle || Qle <-Rest, Qle =< Pivot]) ++ 
	[Pivot] ++ 
	qs_lc([Gr || Gr <-Rest, Gr > Pivot]).

reverse([])->
    [];
reverse([H|Rest]) ->
    reverse([Next || Next <- Rest]) ++ [H].

%max(Set) -> 
%    tail_max(Set,[]).
%tail_max([], Max)->
%    Max;
		
%tail_max([Head|Rest], Max) -> 
   %tail_max(
  %  [Head || Next <- Rest, Head > Next] ++ %, Rest) ++
    %[Max || Next <- Rest, Max>Head, Max>Next]++
%   {Max, Rest} =  tail_max([Next || Next <- Rest, Next > Head, Next > Max], Rest) %++
   %,   tail_max(Rest,Max)
%(Rest) %([Next|| Next <- Rest,  Next >= Head])
%	.

%max([])->
%    [];
%max([Head|Rest]) ->
%    {Max} = get_max(Head, Rest, []),
%    max(Max) .

%get_max(_, [], Max) ->
%    {Max};
%get_max(Head, [H|T], Max) ->
%    if H =< Head ->
%	    get_max(Head, T, [H || Next <- T, H > Max , H > Next]);
%       H > Head -> get_max(Head, T,[H|Max] )
%end.

max([])->
    [];
max([H|T]) -> 
    {Max} = 
	[Next || Next <- T,  Next > H ]% [H || Next <- T,  H > Next]++
    ,max(Max).


%RestaurantMenu= [{steak, 5.99},{ beer, 3.99}, {pouti, 3.50}, {kitt, 20.9}, {water,0.00}].

get_price_with_tax_7 ->
    [{Item, Price*1.07} || {Item, Price} <- RestaurantMenu, Price >=3, Price=<10].
