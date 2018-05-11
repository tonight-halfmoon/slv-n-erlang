%%%-------------------------------------------------------------------
%%% @author rosemary
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. May 2016 6:01 AM
%%%-------------------------------------------------------------------

%%%---------- Problem Statement
%%%       A (rating = 1.2)
%%%      / \
%%%    /     \
%%%   B (2.3) C (3.4)
%%%   \       /
%%%     \   /
%%%      D (4.6)
%%%----------------------------



-module(dag).
-author("rosemary").

%% API
-export([main/0]).
-include_lib("eunit/include/eunit.hrl").

dag_test() ->
  ?assertEqual([], []).

-record(movie, {id='',rating=-0.0}).

main() ->
  Type = [cyclic, private],
  G = digraph:new(Type),
  io:fwrite("Info: ~w~n", [digraph:info(G)]),

  A = add_movie(G, 'A', 1.2),
  B = add_movie(G, 'B', 2.2),
  C = add_movie(G, 'C', 3.4),
  D = add_movie(G, 'D', 4.6),

  add_similar(G, A, B),
  add_similar(G, A, C),
  add_similar(G, B, D),
  add_similar(G, C, D),

  io:fwrite("G's vertices: ~w~n", [digraph:vertices(G)]),

  io:fwrite("A's Out Neighbours: ~w~n", [digraph:out_neighbours(G, A)]),
  io:fwrite("B's Out Neighbours: ~w~n", [digraph:out_neighbours(G, B)]),
  io:fwrite("C's Out Neighbours: ~w~n", [digraph:out_neighbours(G, C)]),
  io:fwrite("D's Out Neighbours: ~w~n", [digraph:out_neighbours(G, D)]),

  A_similars = digraph:out_neighbours(G,A),
  A_sims_IDs = ids(A_similars),
  io:fwrite("A Similars' ID: ~w~n", [A_sims_IDs]),

  B_similars = digraph:out_neighbours(G, B),
  IDs = ids(B_similars),
  io:fwrite("B Similars' ID: ~w~n", [IDs]),


  C_similars = digraph:out_neighbours(G,C),
  C_sims_IDs = ids(C_similars),
  io:fwrite("C Similars' ID: ~w~n", [C_sims_IDs]),

  D_similars = digraph:out_neighbours(G,D),
  D_simis_IDs = ids(D_similars),
  io:fwrite("D Similars' ID: ~w~n", [D_simis_IDs]),

  C_sims_id_rating = tuple_id_rating(C_similars),
  io:fwrite("ID / Rating for C Similars: ~w~n", [C_sims_id_rating]),

  io:fwrite("Info: ~w~n", [digraph:info(G)]),

  io:fwrite("Vertex C: ~w~n", [digraph:vertex(G,C)]),

  Vertices = digraph_utils:preorder(G),
  io:fwrite("G's Vertices /pre-order: ~w~n", [Vertices]),

  Vertices_posorder= digraph_utils:postorder(G),
  io:fwrite("G's Vertices /post-order: ~w~n", [Vertices_posorder]),

  Vertices_topsort = digraph_utils:topsort(G),
  io:fwrite("G's Vertices /topsort: ~w~n", [Vertices_topsort]),


  true.

ids([]) -> [];
ids([#movie{id=ID, rating = _R}|T]) ->
  [ID|ids(T)].

tuple_id_rating([]) ->
  [];
tuple_id_rating([#movie{id=ID, rating=Rating}|T]) ->
  [{ID,Rating}|tuple_id_rating(T)].

add_similar(G, A, B) ->
  digraph:add_edge(G, A, B),
  digraph:add_edge(G, B, A).

add_movie(G, ID, Rating) ->
  digraph:add_vertex(G, #movie{id=ID ,rating=Rating}).
