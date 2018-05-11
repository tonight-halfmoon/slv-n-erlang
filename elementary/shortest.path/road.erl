-module(road).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").


should_report_when_empty_filename_test() ->
    ?assertEqual("emptyFileName!", read("")).

parse_map_test() ->
   ?assertEqual([50,10,30,5,90,20,40,2,25,10,8,0], read("road.txt")).


main() ->
    File = "road.txt",
    group_vals(parse_map(read(File)), []),
    shortest_path(group_vals).

read(FileName) when 0 == length(FileName) ->
    "emptyFileName!";

read(File) ->
    {ok, Binary} = file:read_file(File),
    Binary.

parse_map(Bin) when is_binary(Bin)->
    parse_map(binary_to_list(Bin));
parse_map(Str) when is_list(Str) ->
		    [list_to_integer(X) || X <- string:tokens(Str, "\r\n\t ")].

group_vals([], Acc) ->
  lists:reverse(Acc);
group_vals([A,B,C|Rest], Acc) ->
  group_vals(Rest, [{A,B,C}|Acc]).
    
    
shortest_step({A,B,X}, {{DistA, PathA}, {DistB, PathB}} ) -> 
    DistanceA = {a,A}, {b, B+X},
    DistanceB = {a, A+X},{b, B},
    {erlang:min(), erlang:min()}.

    
