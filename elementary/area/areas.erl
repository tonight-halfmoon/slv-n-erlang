-module(areas).
-export([area/1]).

area({square, Side}) ->
    Side * Side;
area({triangle, A, B, C}) ->
    S = A + B + C /2,
    math:sqrt(S* (S - A)*(S - B)* (S - C));
area({rectangle, Width, Height}) ->
    Width * Height;
area({circle, Radius}) ->
    Radius  * Radius * math:pi();
area(_Other) ->
    {error, invalid_object}.
