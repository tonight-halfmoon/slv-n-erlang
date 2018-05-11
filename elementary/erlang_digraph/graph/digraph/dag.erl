

%%%%%%%%%%%
%% Problem Statemenet
%% Make the following digraph
%%     A  (rating=1.2)
%%   /   \
%%  /     \
%% B(2.3)  C(3.3)
%%  \      /
%%   \    /
%%   D (4.6)
%%
%% A is similar to B and B at the same time is similar to A, and so on
%%%%%%%%%%%

-module(dag).
-export([main/0]).
-include_lib("eunit/include/eunit.hrl").

dag_test () ->
	 ?assertEqual([], []).

-record(movie, {id, rating}).

main() ->
    io:fwrite("Erlang @ Archlinux~n"),
    Type = [cyclic,protected],
    G = digraph:new(Type),
    Movie1 = #movie{id=1, rating=1.2},
    Movie2 = #movie{id=2, rating=2.1},
    Movie3 = #movie{id=3, rating=3.1},
    Movie4 = #movie{id=4, rating=4.6},
    V1 = digraph:add_vertex(G, Movie1),
    V2 = digraph:add_vertex(G, Movie2),
    digraph:add_edge(G,V1 ,V2),
    digraph:add_edge(G, V2, V1), 
    V3 = digraph:add_vertex(G, Movie3),
    V4 = digraph:add_vertex(G, Movie4),
    digraph:add_edge(G, V2, V4),
    digraph:add_edge(G, V3, V4),
    digraph:add_edge(G, V4, V2),
    digraph:add_edge(G, V4, V3),
    digraph:add_edge(G, V3, V1),
    io:fwrite("All edges in G: ~w~n", [digraph:edges(G)]),
    io:fwrite("cycle in G form V1: ~w~n", [digraph:get_cycle(G, V1)]),
    true.
