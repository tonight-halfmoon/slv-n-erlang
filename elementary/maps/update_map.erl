-module(update_map).
-export([start/0]).

start() ->
    M = #{ 1 => ahmad, 2 => taghrid, 3 => germany },
    %updateme(1, a, M).
    %update_all(a, M).
    update_all(a, #{}).
    %update_when_key_1_is_ahmad(a, M).

update_when_key_1_is_ahmad(a, #{1 := ahmad} = M) -> 
    M#{1 := ahmad_updated}.

updateme(K, NewV, Map) ->
    Map#{K => NewV}. 


update_all(_, #{} = M ) ->
    map_is_empty;
update_all(V, M) ->
    update_all2(V, 1, maps:size(M) + 1, M).

update_all(_V, S, S, M) ->
    M;
update_all(V, I, Size, M) ->
    case #{I := _OldV} = M of
	#{I:= _} ->
	    update_all(V, I + 1, Size, M#{I => V});
	_ ->
	    update_all(V, I+1, Size, M)
    end.
	 
update_all2(_V, S, S, M) ->
    M;
update_all2(V, I, Size, #{1 := _} = M) ->
    update_all2(V, I + 1, Size, M#{1 := V}).
