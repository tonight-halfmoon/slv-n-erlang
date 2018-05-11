-module(spawnme).
-include_lib("eunit/include/eunit.hrl").
-export([run/0, read/0, read/1, agent/0]).

%% spawn(spawnme, run, []).
%% spawn(fun read/0).
%% spawn(fun read/1, [V]).

%% 83> spawnme:run()!{spawnme:run()!self(), start, 3}.              
%% Received 'start' token from process <0.347.0>
%% Received Pid: <0.347.0>
%% {<0.347.0>,start,3}
%% function 'start' executed at Var 3
%% 84> 

%% 109> c(spawnme).                   
%% {ok,spawnme}
%% 110> spawn(spawnme, read, [44])!88.
%% Args: 44
%% Received anything else.
%% 88
%%

%%18> c(spawnme).         
%%{ok,spawnme}
%%19> eunit:test(spawnme).
%%  2 tests passed.
%%ok
%%20> 


readStop_asZeroVarReceived_test() ->
    P = spawn(fun read/0),
    ?assertEqual({self(),start,0}, P ! {self(), start, 0}).

readStop_asStopTokenReceived_test () ->
    P = spawn(fun read/0),
    ?assertEqual({self(), stop}, P ! {self(), stop}).

agent()->
    receive
	{PID, Msg} when is_pid(PID) ->
	    io:format("~p: Received a respons from PID `~p`~p~n", [self(), PID, Msg]),
	    {_A,_B,C} = time(),
	    PID ! {self(), start, C  rem 60},
	    agent();
	_ ->
	    ok
	end.

run() ->
    P = spawn(fun read/0),
    io:format("Pid returned by `spawn(fun read/0)` is ~p~n", [P]),
    %P!{self(), start, 5},
    PS = spawn(?MODULE, agent, []),
    io:format("Pid returned by `spawnme, send5msgs, [P`] is ~p~n", [PS])
	,
    PS ! { P, start } 
	.

read() ->
    read(1).
read(Arg) ->
    receive
	{PID, start, 0} when is_pid(PID) ->
	    io:format("Received '0' token from process ~p; Stopped.~n", [PID]),
	    stopped
		;
	{PID, start, Var} when is_pid(PID) ->
	    io:format("Received 'start' token from process ~p~n", [PID]),
	    start(Var),
	    PID!{self(), process_Start_Success}
		;
       	P when is_pid(P) ->
	    io:format("Received Pid: ~p~n", [P])
		;
	{PID, else} ->
	    f(Arg),
	    io:format("Received anything else.~n"),
	    PID!{self(), thanksForVisiting}
    end,
    read(Arg).

f(Arg) ->
    io:format("Args: ~p~n", [Arg]).

start(Var) ->
    io:format("function 'start' executed at Var ~p~n", [Var]),
    ok.
