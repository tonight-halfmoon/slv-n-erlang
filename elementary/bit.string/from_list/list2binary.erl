%%%-------------------------------------------------------------------
%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2017, 
%%% @doc
%%%
%%% @end
%%% On 11 Nov 2017 
%%%-------------------------------------------------------------------
-module(list2binary).
-export([list2binary/1]).
-include_lib("eunit/include/eunit.hrl").

list2binary(L) ->
    list2binary(L, <<>>).

list2binary([H|T], B) when not is_list(H) ->
    list2binary(T, <<H/integer,B/binary>>);
list2binary([H|T], B) when is_list(H) ->
    list2binary(H), list2binary(T, B);
list2binary([], B) ->
    B.
