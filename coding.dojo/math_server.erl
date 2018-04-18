-module(math_server).
-export([start/0, handle_event/1, rpc/2]).

% Demonstrate a very simple server, "Math Server", but not practical!
% Spawn another function, see Emulator input numer 127. And see how the process of the function takes the response from Math Server and prints it formatted. 

%% 125> f().
%% f().
%% ok
%% 126> MSPid = math_server:start().
%% MSPid = math_server:start().
%% <0.308.0>

%% 127> F = spawn(fun() -> receive {Pid, ok, Result} when is_float(Result) -> io:format("~.2f~n", [Result]); M -> io:format("~p~n", M) end end).
%% <0.310.0>
%% 128> MSPid ! {F, [{circle, 3}]}.
%% MSPid ! {F, [{circle, 3}]}.
%% {<0.70.0>,[{circle,3}]}
%% 28.27


%% 134> math_server:rpc(MSPid, [{circle, 3}] ).
%% math_server:rpc(MSPid, [{circle, 3}] ).
%% 28.27
%% ok

start() ->
    spawn(?MODULE, handle_event, [fun geometry:areas/1]).

rpc(ServerPid, Query) ->
    ServerPid ! {self(), Query},
    receive
	{ServerPid, ok, Reply} when is_float(Reply) ->
	    io:format("~.2f~n", [Reply]);
	% Try to uncomment the following pattern and re-compile while the server is running . Reproduce and see that it is executable! Update on-the-fly
	%{ServerPid, ok, Reply} when Reply > 10 ->
	%    io:format("Big Area: ~p~n", [Reply]);
	{ServerPid, ok, Reply} ->
	    io:format("~p~n", [Reply])
        after 5000 ->
	    exit(timeout)
    end.

handle_event(F) ->
    receive
	{From, Query} ->
	    From ! {self(), ok, F(Query)},
	    handle_event(F)
    end.
