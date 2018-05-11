-module(solution).
%-compile(export_all).
-export([main/0, movie_recommends/2]).
-include_lib("eunit/include/eunit.hrl").

-record(movie, {id=0, rating=0.0, similars =[]}).

movie_recommends_test()->
    ?assertEqual([2,4], movie_recommends(1,2)).


movie(Id, Rating) ->
   #movie{id=Id, rating=Rating}.

add_similar(Movie, Sim_movie) ->
    Similars = Movie#movie.similars,
    Movie_updated = Movie#movie{similars =[Sim_movie|Similars]},
    Movie_updated.

%movie_map() -> #{}.


main() ->
    Movie1 =  #movie{id=1, rating=0.02},
    Movie2 = movie(2, 4.5),
    io:fwrite("Movie1: ~w~n", [Movie1]),
    io:fwrite("Movie2: ~w~n", [Movie2]),
    Movie1_updated = add_similar(Movie1,Movie2),
    io:fwrite("Movie1 updated: ~w~n", [Movie1_updated ]),
    Movie_map = #{1 => Movie1_updated , 2 => Movie2},
    io:fwrite("Movie Map: ~w~n", [Movie_map]),
    {ok, Value } = maps:find(1, Movie_map),
    io:fwrite("Find Movie1 in Map: ~w~n", [Value]),
    
    Movie_map_tmp = #{},
    Movie_map_tmp2 = maps:put(1, Movie1_updated, Movie_map_tmp),
    Movie_map2 = maps:put(2, Movie2, Movie_map_tmp2),
    io:fwrite("Movie Map2: ~w~n", [Movie_map2]),
    
    Movie1_updated2 = add_similar(Movie1_updated, movie(3, 3.001)),
    %Movie1_updated_similars = Movie1_updated2#movie.similars,
    
    Movie_map2_updated = maps:update(1, Movie1_updated2, Movie_map2 ),
    io:fwrite("Movie Map2 updated: ~w~n", [Movie_map2_updated]),

    true.

movie_recommends(_Id, _Params) ->
    ok.
