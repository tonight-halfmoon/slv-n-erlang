-module(len_of_list_).
-export([len/1]).

len([])    -> 0;
len([AFFFF|T]) ->  1 + len(T).
