%%%-------------------------------------------------------------------
%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2017, 
%%% @doc
%%%
%%% @end
%%% On 11 Nov 2017 
%%%-------------------------------------------------------------------
-module(binary2list).
-export([binary2list/1]).
-include_lib("eunit/include/eunit.hrl").

binary2list(B) ->
    binary2list(B, []).
binary2list(<<X/integer, T/binary>>, L) ->
    binary2list(T, [X|L]);
binary2list(<<>>, L) ->
    L.


