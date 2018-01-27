%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2018, 
%%% @doc
%%% Count all pairs where Xi =:= Xj, given j =:= i + 1
%%% @end
%%% On the 26th of Jan 2018

-module(cap).
-export(do/1).

do(L) ->
    count_adjacent_pairs(L, 0).

count_adjacent_pairs([], Acc) ->
    Acc;
count_adjacent_pairs([X], Acc) ->
    Acc;
count_adjacent_pairs([X, X|T], Acc) ->
    count_adjacent_pairs([X|T], Acc + 1);
count_adjacent_pairs([_X, Y|T], Acc) ->
    count_adjacent_pairs([Y|T, Acc]).
