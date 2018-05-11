-module(db).
-export([new/0, destroy/1, write/3]).

new() ->
    file:open("dbp.db", [write]),
    [].

destroy(Db) ->
    ok.

write(Key, Element, Db) ->    
    NewDb = [{Key, Element}|Db],
    F = file:open("dbp.db", [write]),
    file:write(F, list_to_binary(Element)),
    NewDb.
