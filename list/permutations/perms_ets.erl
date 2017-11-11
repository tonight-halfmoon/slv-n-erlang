%%%-------------------------------------------------------------------
%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2017, 
%%% @doc
%%% Generate Perumtations of a list utilising ETS
%%% @end
%%% On the 5th of November 2017 
%%% Last updated on the 11th of November 2017
%%%-------------------------------------------------------------------
-module(perms_ets).
-export([start/1, permute_2ets/3, startk/4]).
-include_lib("eunit/include/eunit.hrl").
-define(WD, "./work").
-define(Tabfile, "permstab").
-define(Star, "*").
-define(Slash, "/").
-define(Underscore, "_").

clean() ->
    FileList = filelib:wildcard(lists:concat([?Tabfile, ?Star]), ?WD),
    lists:map(fun(File) -> file:delete(list_to_atom(lists:concat([?WD, ?Slash, File]))) end, FileList),
    file:del_dir(?WD),
    true.

start([]) ->
    [[]];
start(L) when is_list(L) -> 
    clean(),
    file:make_dir(?WD),
    %permute_2ets(L, "./work/permstab", pt).
    Tid = ets:new(pt, [set, public, {write_concurrency, true}, {read_concurrency, true}]),
    Pid = spawn(?MODULE, permute_2ets, [L, lists:concat([?WD, ?Slash, ?Tabfile]), Tid]),
    clean(),
    Pid.

startk(_, [], _, _) ->
    [[]];
startk(K, L, File, _Tab) when is_list(L) -> 
    %file:delete(File),
    Filen = lists:concat([File, ?Underscore, K]),
    %Tabn = lists:concat([Tab, "_", K]),
    %ets:new(list_to_atom(Tabn), [set, named_table]),
    Tid = ets:new(tabn, [set, public, {write_concurrency, true}, {read_concurrency, true}]),
    permute_k2ets(K, L, Filen, Tid).

permute_2ets([], _, _) ->
    [[]];
permute_2ets([X], _, Tab) ->
    ets:delete(Tab),
    [[X]];
permute_2ets([X,Y], _, Tab) ->
    ets:delete(Tab),
    [[X,Y],[Y,X]];
permute_2ets(L, File, Tab) ->
    %ets:new(pt, [set, named_table]), %%set
    permute_2ets(L, L, File, Tab).

permute_2ets(L, [X|T], File, Tab) ->
    ets:insert(Tab, {X, L--[X]}),
    permute_2ets(L, T, File, Tab);
permute_2ets(_, [], File, Tab) ->
    ets:tab2file(Tab, File, [{sync, true}]), 
    ets:delete(Tab),
    conq_2ets(File, Tab).

permute_k2ets(_, [], _, _) ->
    [[]];
permute_k2ets(K, [X], File, Tab) ->
    Tent = [[X]],
    %ets:new(Tab, [set, named_table]),
    ets:insert(Tab, {K, Tent}),
    ets:tab2file(Tab, File, [{sync, true}]),
    ets:delete(Tab);
permute_k2ets(K, [X,Y], File, Tab) ->
    Tent = [[X,Y],[Y,X]],
    %ets:new(Tab, [set, named_table]),
    ets:insert(Tab, {K, Tent}),
    ets:tab2file(Tab, File, [{sync, true}]),
    ets:delete(Tab);
permute_k2ets(K, L, File, Tab) ->
    %ets:new(Tab, [set, named_table]),
    permute_k2ets(K, L, L, File, Tab).

permute_k2ets(K, L, [H|T], File, Tab) ->
    ets:insert(Tab, {H, L--[H]}),
    permute_k2ets(K, L, T, File, Tab);
permute_k2ets(_, _, [], File, Tab) ->
    ets:tab2file(Tab, File, [{sync, true}]),
    ets:delete(Tab),
    conq_2ets(File, Tab).

conq_2ets(File, Tab) ->
    case ets:file2tab(File) of
	{ok, Rt} ->
            file:delete(File),
	    L = ets:tab2list(Rt),
	    ets:delete(Rt),
	    conq_k2ets(L, File, Tab);
	{error, _Reason} ->
	    not_interested
    end.

conq_k2ets([], _, _) ->
    [];
conq_k2ets([{K,V}|T], File, Tab) ->
    spawn(?MODULE, startk, [K, V, File, Tab]),
    %startk(K, V, File, Tab),
    conq_k2ets(T, File, Tab).

permute_empty_test_() ->
    {
      "Permute an Empty list yields into an empty list",
      ?_assertEqual([[]], start([]))
    }.

permute_1_test_() ->
    ?_assertMatch(Pid when is_pid(Pid), start([1])).

permute_2_test_() ->
    ?_assertMatch(Pid when is_pid(Pid), start([1,2])).

perms_r2_test_() ->
    ?_assertMatch(Pid when is_pid(Pid), start([2,1])).

permute_9elem_list_test_() ->
    {
      "Permute 9-element list must pass",
      ?_assertMatch(Pid when is_pid(Pid), perms_ets:start(lists:seq(1, 9)))
    }.

permute_10elem_list_test_() ->
    {
      "Permute 10-element list must pass",
      ?_assertMatch(Pid when is_pid(Pid), perms_ets:start(lists:seq(1, 10)))
    }.

permute_11elem_list_test_() ->
    {
      "Permute 11-element list must pass",
      ?_assertMatch(Pid when is_pid(Pid), perms_ets:start(lists:seq(1, 11)))
    }.

permute_15elem_list_test_() ->
    {
      "Permute 15-element list must pass",
      ?_assertMatch(Pid when is_pid(Pid), perms_ets:start(lists:seq(1, 15)))
    }.

permute_500elem_list_test_() ->
    {
      "Permute 500-element list must pass",
      ?_assertMatch(Pid when is_pid(Pid), perms_ets:start(lists:seq(1, 500)))
    }.

permute_1399elem_list_test_() ->
    {
      "Permute 1399-element list must pass",
      ?_assertMatch(Pid when is_pid(Pid), perms_ets:start(lists:seq(1, 1399)))
    }.

%% permute_1599elem_list_test_() ->
%%     {
%%       "Permute 1599-element list must pass",
%%       ?_assertMatch(Pid when is_pid(Pid), perms_ets:start(lists:seq(1, 1599)))
%%     }.

%% permute_1699elem_list_test_() ->
%%     {
%%       "Permute 1699-element list must pass",
%%       ?_assertMatch(Pid when is_pid(Pid), perms_ets:start(lists:seq(1, 1699)))
%%     }.


%% permute_1999elem_list_test_() ->
%%     {
%%       "Permute 1999-element list must pass",
%%       ?_assertMatch(Pid when is_pid(Pid), perms_ets:start(lists:seq(1, 1999)))
%%     }.


%% permute_2500elem_list_test_() ->
%%     {
%%       "Permute 2500-element list must pass",
%%       ?_assertMatch(Pid when is_pid(Pid), perms_ets:start(lists:seq(1,2500)))
%%     }.
