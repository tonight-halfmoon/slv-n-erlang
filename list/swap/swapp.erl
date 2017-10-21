%%%-------------------------------------------------------------------
%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2017, 
%%% @doc
%%%
%%% @end
%%% Created : 20 Oct 2017 by  <rosemary@SCUBA>
%%%-------------------------------------------------------------------

-module(swapp).
-include_lib("eunit/include/eunit.hrl").
-export([swp/1]).

-author('rosemary@SCUBA').

swp([]) ->
    [];
swp([A]) ->
    [A];
swp([X,Y]) ->
    [[Y,X], [X,Y]];
swp([X,Y,Z]) ->
    [H1,H2]= lists:map(fun(P) -> [X|P] end, swp([Z,Y])),
    [H3,H4]= lists:map(fun(P) -> [Y|P] end, swp([X,Z])),
    [H5,H6]= lists:map(fun(P) -> [Z|P] end, swp([Y,X])),
    [H1,H2,H3,H4,H5,H6];
swp(L) ->
    swp(L, []).

swp([], S) ->
    S;
swp([X,Y,Z|[H|T]], S) ->
   swp(T, 
       lists:append(
	 lists:append(
	   lists:append(
	     lists:map(fun(P) -> [X|P] end, swp([H,Y,Z])),
	      lists:map(fun(P) -> [Y|P] end, swp([X,H,Z]))),
	   lists:append(
	     lists:map(fun(P) -> [Z|P] end, swp([X,Y,H])),
	      lists:map(fun(P) -> [H|P] end, swp([X,Y,Z])))),
	 S)
      );
swp([X|T], S) ->
    swp(T, lists:map(fun(P) -> [X|P] end, S)).
