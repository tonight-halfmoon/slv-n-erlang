-module(spec).
-export([a/1]).

-spec( a(atom()) -> float()).
-type point()    :: {integer(), integer()}.

a(Point) ->
    Point.
