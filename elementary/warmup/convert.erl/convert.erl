-module(convert).
-export([convert/1]).

convert({centimeter, Z}) ->
    {inch, Z/2.54};
convert({inch,Y}) ->
    {centimeter, Y * 2.54}.
