-module(dates).
-export([classify_day/1]).

classify_day(saturday) ->
 {saturday, "is a weekend day"};

classify_day(sunday) ->
 {sunday, "is a weekend day"};

classify_day(Day) ->
   case work_day(Day) of
       true ->
	   {Day, "is a business day"};
       false ->
	  classify_day(Day) % endless loop with any input different than then 7 days of week.
   end.

work_day(Day) ->
    work_day(Day, [monday, tuesday, wednesday, thursday, friday]).

work_day(_, []) ->
    false;
work_day(Day, [NexWorkDay|RestWorkdays]) ->
    if Day =:= NexWorkDay ->
	    true;
       Day /= NexWorkDay ->
	    work_day(Day, RestWorkdays)
    end.
