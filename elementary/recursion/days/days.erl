-module(days).
-export([classify_day/1]).

classify_day(saturday) ->
	{saturday, " is a weekend day."};

classify_day(sunday) ->
	{sunday, " is a weekend day."};

classify_day(Day) ->
	classify_day(Day, [monday, tuesday, wednesday, thursday, friday]).

classify_day(Entry, []) ->
    {Entry, " is not a day!"};

classify_day(Day, WorkdayList) ->
    [H|Res] = WorkdayList,
    if Day == H ->
	    {Day, "is a work day."};
       Day /= H ->
	    classify_day(Day, Res)
    end. 
		

