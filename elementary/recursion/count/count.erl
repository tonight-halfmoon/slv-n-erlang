-module(count).
-export([count/1]).


count([])-> 
    0;
count([_|T]) ->
    count(T)+1.
