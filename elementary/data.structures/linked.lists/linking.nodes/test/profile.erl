-module(profile).

-export([profile/3]).

profile(Module, Fun, Arguments) ->
    {Microseconds, _out} = timer:tc(Module, Fun, Arguments),
    io:fwrite("Execution time: ~w microseconds~n", [Microseconds]),
    Microseconds.
