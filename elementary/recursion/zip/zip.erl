-module(zip).
-export([zip/2]).

%zip([],[]) -> [];  % the following two base cases suffice
zip([],_) -> [];
zip(_,[]) -> [];
zip([H|T],[U|Y]) ->
    [{H,U}|zip(T,Y)].
    
