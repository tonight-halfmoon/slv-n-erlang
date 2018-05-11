-module(len_of_list).
-export([len/1]).

len([]) -> 0;
len([_|T]) -> 1 + len(T).
%% the length of the head plus the length of the rest