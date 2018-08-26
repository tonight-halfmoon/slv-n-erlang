-module(record_bifs).

-export([test/1]).

-record(person, {name, age=0}).

test(Args) ->
    case is_record(Args, person) of
	true ->
	    io:format("Args is a record value ~p~n", [Args]);
	false ->
	   io:format("Args is not a record. Args value ~p~n", [Args])
    end.
