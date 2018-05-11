-module(remove).
-export([remove/2]).


remove([],_)->
    [];
remove(S, 0) ->
    S;
remove([_|T], N)->
 	remove(T, N -1).
