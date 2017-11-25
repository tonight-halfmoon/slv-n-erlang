-module(clean_dir).
-export([clean]).

clean(Dir) ->
    FileList = filelib:wildcard("*.*", Dir),
    lists:map(fun(File) -> file:delete(list_to_atom(lists:concat([Dir, File]))) end, FileList),
    file:del_dir(Dir),
    true.

