-module(rot13).
-export([rot13/0]).

rot13() ->
  case io:get_chars('', 8192) of
    eof -> init:stop();
  Text ->
    Rotated = [rot13(C) || C <- Text],
    io:put_chars(Rotated),
    rot13()
end.

rot13(C) when C >= $a, C =< $z -> $a + (C - $a + 13) rem 26;
rot13(C) when C >= $A, C =< $Z -> $A + (C - $A + 13) rem 26;
rot13(C) -> C.