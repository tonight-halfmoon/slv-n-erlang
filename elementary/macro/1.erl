-module(macro_drill0).
-export([use/2]).

-define(empty_string, "").
-define(sub(M,N), M - N).

use(M,N) ->
    ?sub(M,N).
