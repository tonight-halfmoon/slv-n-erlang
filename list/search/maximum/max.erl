-module(max).
-export([maximum/1, maxdc/1, maxdc2/1, maxdc_tail/1, maxdc_tail2/1]).
-include_lib("eunit/include/eunit.hrl").
-include("./include/testcase.01.hrl").
-import(split2, [split/1]).

%%% Providing 5 different implementaions

%% simple implementation in tail recursion (The fastest!).
maximum([]) ->
    'List is empty';
maximum([H|T]) ->
    maximum(T, H).

maximum([], Mx) ->
    Mx;
maximum([H|T], Mx) ->
    maximum(T, max(H, Mx)).
 
%% Implement divide and conquer algorithm; using erlang:max and max:maximum; does not scale for very big lists ~ 10^5 elements
maxdc([]) ->
    'List is empty';
maxdc(L) ->
    {[HFh|TFh], [HSh|TSh]} = lists:split(trunc(length(L)/2), L),
    max(maximum(TFh, HFh), maximum(TSh, HSh)).

%% Implement divide and conquer algorithm in tail recursion
maxdc_tail([])->
    'List is empty';
maxdc_tail(L) ->
    maxdc_tail(L, 0).

maxdc_tail([], M1) ->
    M1;
maxdc_tail(L, M1) ->
    {L1, L2} = lists:split(trunc(length(L)/2), L),
    case {L1, L2} of 
	{[], []}->
	    M1;
	{[], [H2|T2]} ->
	    max(M1, maxdc_tail(T2, max(M1, H2)));
	{[H1|T1], []} ->
	    max(M1, maxdc_tail(T1, max(M1, H1)));
	{[HFh|TFh], [HSh|TSh]} ->
	    max(maxdc_tail(TFh, max(M1, HFh)), maxdc_tail(TSh, max(M1, HSh)))
    end.

%% Implement divide and conquer algorithm in tail recursion with 2 accumulators
maxdc_tail2([])->
    'List is empty';
maxdc_tail2(L) ->
    maxdc_tail2(L, 0, 0).

maxdc_tail2([], M1, M2) ->
    max(M1, M2);
maxdc_tail2(L, M1, M2) ->
    %{L1, L2} = lists:split(trunc(length(L)/2), L),
    {L1, L2} = split2:split(L),
    case {L1, L2} of 
	{[], []}->
	    maxdc_tail2([], M1, M2);
	{[], [H2|T2]} ->
	    maxdc_tail2([], M1, maxdc_tail2(T2, M1, max(M2, H2)));
	{[H1|T1], []} ->
	    maxdc_tail2(T1, max(M1, H1), M2);
	{[HFh|TFh], [HSh|TSh]} ->
	    maxdc_tail2(TFh, max(M1, HFh), maxdc_tail2(TSh, M1, max(M2, HSh)))
    end.

%% Implement divide and conquer algorithm; Utilises erlang:max.
maxdc2([]) ->
    'list is empty';
maxdc2([X|[]]) ->
    X;
maxdc2(L) ->
    %{Left, Right} = split2:split(L),
    {Left, Right} = lists:split(trunc(length(L)/2), L),
    max(maxdc2(Left), maxdc2(Right)).


