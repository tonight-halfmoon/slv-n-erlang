-module(udp_client).
-export([client/1]).

%% How to run
%% (0) Start a UDP Server on port 3456.
%% (1) Compile the UDP client
%% 3> c(udp_client).                
%% {ok,udp_client}
%% 4> udp_client:client(<<"Help">>).
%% {ok,<<"THIS IS BINARY from ERLANG!">>}
%% 5> 


%% 
%%Erlang/OTP 19 [erts-8.3] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]

%%Eshell V8.3  (abort with ^G)
%%1> c(udp_client).
%5{ok,udp_client}
%%2> udp_client:client(40).
%%client opened socket=#Port<0.2116>
%%client received:{udp,#Port<0.2116>,
%%                     {127,0,0,1},
%%                     4000,
%%                     <<131,110,20,0,0,0,0,0,64,37,5,255,100,222,15,8,126,242,
%%                       199,132,27,232,234,142>>}
%% 815915283247897734345611269596115894272000000000

client(N) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    io:format("client opened socket=~p~n", [Socket]),
    Ref = make_ref(), %% make a unique reference
    B1 = term_to_binary({Ref, N}),
    ok = gen_udp:send(Socket, "localhost", 4000, B1), %%term_to_binary(N)),
    Correct_val = wait_for_ref(Socket, Ref),
    io:format("client received:~p~n", [Correct_val]),
    gen_udp:close(Socket)
	.

wait_for_ref(Socket, Ref) ->
    receive
	{udp, Socket, _, _, Bin} ->
	    case binary_to_term(Bin) of
		{Ref, Val} ->
		    %% got the correct value
		    Val;
		{_SomeOtherRef, _} ->
		    %% some other value throw it away
		    wait_for_ref(Socket, Ref)
		    %% binary_to_term(Bin)
	    end
    after 1000 ->
	    error
    end.
