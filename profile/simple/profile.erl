-module(profile).
-export([profile/3]).

profile(Module, Fun, L) ->
    {Microseconds, _out} = timer:tc(Module, Fun, [L]),
    io:fwrite("Execution time: ~w microseconds~n", [Microseconds]).

