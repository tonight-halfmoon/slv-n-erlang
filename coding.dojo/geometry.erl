-module(geometry).
-export([areas/1, apply_formatted/2]).

%% rosemary@SCUBA:[226]~/..rlang/cdojo$ erl
%% Erlang/OTP 20 [erts-9.3] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:10] [hipe] [kernel-poll:false]

%% Eshell V9.3  (abort with ^G)
%% 1> c(geometry).
%% {ok,geometry}
%% 2> geometry:
%% areas/1             formatted_output/2  module_info/0       
%% module_info/1        
%% 2> geometry:apply_formatted(fun geometry:areas/1, [{circle,3}]).
%% 28.27
%% ok
%% 3> geometry:apply_formatted(fun geometry:areas/1, [{square,3}]).
%% 9
%% ok
%% 4> q().
%% ok


apply_formatted(F, L) ->
    Result = F(L),
    case is_float(Result) of
	true ->
	    io:format("~.2f~n", [Result]);
	false ->
	    io:format("~p~n", [Result])
    end.

areas(L) ->
    lists:sum(
      lists:map(fun(I) -> area(I) end, L)).

area({square, Side}) ->
    Side * Side;
area({rectangle, W, H}) ->
    W * H;
area({circle, R}) ->
    math:pi() * R * R;
area({triangle, A, B, C}) ->
    S = (A + B + C) / 2,
    math:sqrt(S * (S-A) * (S-B) * (S-C)).
