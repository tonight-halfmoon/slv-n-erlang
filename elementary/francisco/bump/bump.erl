-module(bump).
-export([bump/1]).

bump([]) -> [];
bump([H|T]) ->
	[H+1|bump(T)].

