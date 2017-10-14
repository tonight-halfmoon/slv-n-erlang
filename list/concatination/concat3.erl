%%%-------------------------------------------------------------------
%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2017, 
%%% @doc
%%%
%%% @end
%%% Created :  8 Oct 2017 by  <rosemary@SCUBA>
%%%-------------------------------------------------------------------
-module(concat3).
-author('rosemary@SCUBA').
-export([concat/3]).

concat(L1, L2, L3) when is_list(L2) ->
    [L1|[L3|L2]];
concat(L1, L2, L3) when is_list(L3) ->
    [L2|[L1|L3]];
concat(L1, L2, L3) when is_list(L1) ->
    [L2|[L3|L1]];
concat(A, B, C) ->
    concat(B, A, [C]).

