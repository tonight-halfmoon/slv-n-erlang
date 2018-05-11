%%% @author  <rosemary@SCUBA>
%%% @copyright (C) 2017, 
%%% @doc
%%% RESA Server written as a special process using sys and proc_lib modules 
%%% complies to the OTP design principles without using a standard behaviour.
%%% This version of RESA Server process is to:
%%%  - Be started in a way that makes the process fit into a supervision tree
%%%  - Support the sys debug facilities
%%%  - Take care of system messages. 
%%% @end
%%% On the 5th of December 2017

-module(resa_server_special_process).
-export([start_link/0]).
-export([alloc/0, free/1]).
-export([init/1]).
-export([system_continue/3, system_terminate/4,
	write_debug/3,
	system_get_state/1, system_replace_state/2]).

start_link() ->
    proc_lib:start_link(resa_server, init, [self()]).

alloc() ->
    resa_server ! {self(), alloc},
    receive
	{resa_server, Res} ->
	    Res
	end.

free(Ch) ->
    resa_server ! {free, Ch},
    ok.

init(Parent) ->
    register(resa_server, self()),
    Chs = channels(),
    Deb = sys:debug_options([]),
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(Chs, Parent, Deb).

loop(Chs, Parent, Deb) ->
    receive
	{From, alloc} ->
	    Deb2 = sys:handle_debug(Deb, fun resa_server:write_debug/3,
				   resa_server, {in, alloc, From}),
	    {Ch, Chs2} = alloc(Chs),
	    From ! {resa_server, Ch},
	    Deb3 = sys:handle_debug(Deb2, fun resa_server:write_debug/3,
				    resa_server, {out, {resa_server, Ch}, From}),
	    loop(Chs2, Parent, Deb3);
	{free, Ch} ->
	    Deb2 = sys:handle_debug(Deb, fun resa_server:write_debug/3,
				    resa_server, {in, {free, Ch}}),
	    Chs2 = free(Ch, Chs),
	    loop(Chs2, Parent, Deb2);
	{system, From, Request} ->
	    sys:handle_system_msg(Request, From, Parent, resa_server,
				  Deb, Chs)
	end.

system_continue(Parent, Deb, Chs) ->
    loop(Chs, Parent, Deb).

system_terminate(Reason, _Parent, _Deb, _Chs) ->
    exit(Reason).

system_get_state(Chs) ->
    {ok, Chs}.

system_replace_state(StateFun, Chs) ->
    NChs = StateFun(Chs),
    {ok, NChs, NChs}.

write_debug(Dev, Event, Name) ->
    io:format(Dev, "~p event = ~p~n", [Name, Event]).


alloc({Allocated, [H|T] = _Free}) ->
    {H, {[H|Allocated], T}}.

free(Ch, {Alloc, Free} = Channels) ->
    case lists:member(Ch, Alloc) of
	true ->
	    {lists:delete(Ch, Alloc), [Ch|Free]};
	false ->
	    Channels
    end.

channels() ->
    {_Allocated = [], _Free = lists:seq(1, 100)}.
