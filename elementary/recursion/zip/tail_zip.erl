-module(tail_zip).
-export([zip/2]).

zip(T,S) ->
    tail_zip(T,S,[]).

tail_zip([],[],ZPD) ->
    ZPD;
tail_zip([H|T],[U|Y], ZPD) ->
    tail_zip(T,Y,[{H,U}|ZPD]).
