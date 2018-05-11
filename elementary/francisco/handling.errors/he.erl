

41> try(X = 3) of Val -> {normal, Val} catch _:_ -> {error, {"X is already bound to value: ", X}} end.
{error,{"X is already bound to value: ",2}}
