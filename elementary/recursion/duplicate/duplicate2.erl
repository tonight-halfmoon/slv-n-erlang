-module(duplicate2).
-export([dup/2]).
-include_lib("eunit/include/eunit.hrl").
%dup(List, 0) ->
%    List;
%dup([], _) ->
%    [];
%dup(List, Times) -> 
%    dup(List, Times, List).

%dup(_, 0, Output) ->
%    Output;
%dup(List, Times, Output) ->
%    dup(List, Times -1 , Output++List).

%dup(_, N) 
%  when N < 0 ->
%    io:format("Parameter Times must be integer ~n");
%dup(_, Times) 
%  when not(is_integer(Times)) ->
%    io:format("Times must be integer~n");
%dup(List, _) 
%  when not(is_list(List)) ->
%    io:format("List must be list~n");
dup(List, 0 )->
    List;
dup(List, Times) -> % when is_integer(Times) andalso is_list(List) ->
    ?assert(is_integer(Times)),
    ?assertNot(Times<0),
    ?assert(is_list(List)),
    dup(List, Times, List).
dup(_, 0, Output) ->
    Output;
dup(List, Times, Output) ->
    dup(List, Times - 1, List ++ Output).



