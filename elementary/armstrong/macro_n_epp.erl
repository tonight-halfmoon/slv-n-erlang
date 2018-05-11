-module(macro_n_epp).

-export([what/0]).

-define(macro1, 'thisisareplacementofmacro1').

what() ->
    ?LINE,
    ?macro1.
