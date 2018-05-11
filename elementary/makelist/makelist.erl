-module(makelist).
-export([mklist/1]).

%% make list of K random numbers
%% Each random number in the range 1..1000000

mklist(K)->
    mklist(K, []).

mklist(0, L) -> L;
mklist(K, L) -> mklist(K-1, [random:uniform(1000000)|L]).
    
