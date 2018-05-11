%29th Dec, 
%2015 ,
 % Dammam SA, 

-module(remove).
-export([remove/2]).

% still does not check if N is already greater than the length of the original passed list
remove(Set, 0) ->
    Set;
remove([], _) ->
    [];
remove([_|T], N) -> % this is solved with the help of list comprehensions!
    [Nex || Nex<- T],
    remove(T, N -1).


% the following has a bug that it won't consider when the list becomes empty!!
% so it does no go  back to the base case aboe
%remove([_|T], N) 
%  when N >= 1 ->
%    [H|TT] = T, TT,
%	remove(T, N -1).

%remove([_, Nex|T], N) 
%  when N >= 1 ->
%    [Nex] ++ 
%	remove(T, N -1).
