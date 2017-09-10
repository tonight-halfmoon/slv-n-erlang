-module(rw_file).
-export([read/1, write/2]).

%%% How 2 Use
%%% Let's say you want the second test case input which is the following 100000 elements from testcase_input02.
%%% rw_file:write("./out.md", lists:sublist(rw_file:read("testcase_input02.txt"), 200016, 100000)).
%%% io:fwrite("~w~n", [lists:sublist(read_testcase_input:read("testcase_input02.txt"),14,100000 )]).

read(File) ->
    {ok, Binary} = file:read_file(File),
    parse(Binary).

parse(Binary) ->
    List = binary_to_list(Binary),
    [list_to_integer(X) || X <- string:tokens(List, "\r\n\t ")].

write(File, List) ->
    file:write_file(File, to_binary(List)).

to_binary(L) ->
    to_binary(L, <<>>).

to_binary([], BinaryL) ->
    [<<"[">>, BinaryL, <<"]">>];
to_binary([H|[]], BinaryL) ->
    to_binary([], [integer_to_binary(H)|BinaryL]);
to_binary([H|T], BinaryL) ->
    to_binary(T, [<<",">>, integer_to_binary(H)|BinaryL]).



%%print_first102_values(File) ->
%%    L = read(File),
%%   lists:sublist(L, 102).
