-module(build_index).

-export([index/1]).

-define(TableName, indexTable).
-define(Punctuation, "(\\ |\\,|\\.|\\;|\\:|\\t|\\n|\\(|\\))+").

index(File) ->
    ets:new(?TableName, [ordered_set, named_table]),
    processFile(File),
    prettyIndex().

processFile(File) ->
    {ok, IoDevice} = file:open(File, [read]),
    processLines(IoDevice, 1).

processLines(IoDevice, N) ->
    case io:get_line(IoDevice, "") of
	eof ->
	    ok;
	Line ->
	    processLine(Line, N),
	    processLines(IoDevice, N + 1)
    end.

processLine(Line, N) ->
    case re:split(Line, ?Punctuation) of
	[<<>>] ->
	    [];
	Words ->
	    processWords(Words, N)
    end.

processWords(Words, N) ->
    case Words of
	[] ->
	    ok;
	[BinWord|Rest] ->
	    Word = binary_to_list(BinWord),
	    if
		length(Word) > 3 ->
		    Normalise = string:to_lower(Word),
		    ets:insert(?TableName, {{Normalise, N}});
		true ->
		    ok
	    end,
	    processWords(Rest, N)
    end.

prettyIndex() ->
    case ets:first(?TableName) of
	'$end_of_table' ->
	    ok;
	First ->
	    case First of
		{Word, N} ->
		    IndexEntry = {Word, [N]}
	    end,
	    prettyIndexNext(First, IndexEntry)
    end.

prettyIndexNext(Entry, IndexEntry = {Word, N}) ->
    Next = ets:next(?TableName, Entry),
    case Next of
	'$end_of_table' ->
	    prettyEntry(IndexEntry);
	{NextWord, M} ->
	    if
		NextWord == Word ->
		    prettyIndexNext(Next, {Word, [M|N]});
		true ->
		    prettyEntry(IndexEntry),
		    prettyIndexNext(Next, {NextWord, [M]})
	    end
    end.

prettyEntry({Word, Lines}) ->
    io:format("~p\t\t",[Word]),
    prettyEntryLines(Lines).

prettyEntryLines([]) ->
    io:format("~n"),
    ok;
prettyEntryLines([X]) ->
    io:format(" ~p", [X]),
    prettyEntryLines([]);
prettyEntryLines([Line|Rest]) ->
    io:format("~p,", [Line]),
    prettyEntryLines(Rest).
