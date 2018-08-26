-module(enumerated_types).

-export([day_of_week/1]).

-define(WEEK_DAYS, ["MON", "TUE", "WED", "THU", "FRI", "SAT", "SON"]).
-define(DAY_OF_WEEK(I), lists:nth(I, ?WEEK_DAYS)).

day_of_week(Args) ->
    ?DAY_OF_WEEK(Args).
