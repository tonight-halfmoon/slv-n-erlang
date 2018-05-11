
splitmap_test()->
    ?assertEqual({[1],[2]}, splitmap(fun split_eoindx:split/1, [1,2])).

splitmap(F, L)->
    F(L).
