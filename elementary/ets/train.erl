insert_ets([], []) ->
   % traverse_ets2();
    ets:tab2list(pt);
insert_ets([X|Xs], [Tent|Tents]) ->
    %io:fwrite(" X ~w~n", [X]),
    %io:fwrite(" Tent ~w~n", [Tent]),
    ets:insert(pt, {X, Tent}),
    insert_ets(Xs, Tents).

traverse_ets2(M) ->
    traverse_ets2(M, 0, []).

traverse_ets2(M, M, L) ->
    L;
traverse_ets2(M, I, L) ->
    traverse_ets2(M, I + 1, [ets2list(ets:lookup(pt, I + 1), [])|L]).

traverse_ets() ->
    First = ets:first(pt),
    io:fwrite(" Key First: ~w~n", [First]),
    case First of 
	'$end_of_table' ->
	    first_is_empty;
	Key ->
	    ets2list(ets:lookup(pt, Key), []),
	    printrest(Key)
    end.

printrest(Key1) ->
    case ets:next(pt, Key1) of
	'$end_of_table' ->
	    next_is_empty;
	Key ->
	    ets2list(ets:lookup(pt, Key), []),
	    printrest(Key)
    end.

join(Xs, Tents) ->
   join(Xs, Tents, []).

join ([X|Xs], [Tent|Tents], Ps) ->
    io:fwrite(" >>>~n join~n"),
    io:fwrite(" Ps: ~w~n", [Ps]),
    io:fwrite(" [Tent|Tents]: ~w~n", [[Tent|Tents]]),
    io:fwrite(" [X|Xs]: ~w~n", [[X|Xs]]),
    %P = [[X|Y] || Y <- permute(Tent)],
    
    P = lists:map( fun(Y) -> lists:map(fun(Z) -> [X,Z] end, Y) end, permute(Tent)),
    %io:fwrite(" P: ~w~n", [P]),
    %[ets:insert(pt, {1, [X|Y]}) || Y <- permute(Tent)],
    join(Xs, Tents, [P|Ps]);
join ([], [], Ps) ->
    Ps.


ets2list([], L) ->
    L;
ets2list([H|T], L) ->
    case H of 
	{_K, V} ->
	    ets2list(T, [V|L]);
	_ ->
	    not_interested
    end.
