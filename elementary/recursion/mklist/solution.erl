
-module(solution).
-export([main/0]).


    
main() ->
    {ok, [N]} = io:fread("", "~d"),
% Fill up these questions marks to call a function (written by you)
% Which creates an array with N elements     
    Arr = mklist(N),
   io:format("~B~n", [length(Arr)]).
% Do not change the lines of code already present.
% That is to assist us in evaluating whether the array you created
% has, indeed N elements.

mklist(N) ->
       mklist(N, 0, []).
mklist(N,N,L)->
       L;
mklist(0,_, _) ->
      [];
mklist(N, I, L) ->
      mklist(N, I+1, [I|L]).
